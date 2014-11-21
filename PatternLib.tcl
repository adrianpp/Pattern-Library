namespace eval HA_BACKEND {}

proc HA_BACKEND::getBuildPath {} {
	if {[info exists primAlgorithm::libroot]} {
		return $primAlgorithm::libroot
	}
	return "/home/adrian/workspace/build/"
}

proc HA_BACKEND::load_pattern_file {patternFileName} {
	set myslave [interp create]
	$myslave eval "load [getBuildPath]/lib/esthelper.so"
	$myslave eval "source [getBuildPath]/Scripts/Ilds/PrimAlgorithms/netlist_on_cdfg.tcl"
	$myslave eval "source $patternFileName"
	return $myslave
}

#should only be necessary for debugging purposes
proc get_cdfg_from_file {cdfgFileName} {
	load [HA_BACKEND::getBuildPath]/lib/esthelper.so
	puts stdout "[HA_BACKEND::getBuildPath]/Scripts/Ilds/PrimAlgorithms/netlist_on_cdfg.tcl"
	source [HA_BACKEND::getBuildPath]/Scripts/Ilds/PrimAlgorithms/netlist_on_cdfg.tcl
  rename load temp__load
  proc load {file} {}
	source $cdfgFileName
	rename load ""
	rename temp__load load
	return $cdfg
}

proc HA_BACKEND::getFuncNameRaw {{index -1}} {
	array set a [info frame $index]
	if {[info exists a(proc)]} {
    return $a(proc)
	}
	return ""
}

proc HA_BACKEND::getFuncName {{index -2}} {
	return [regsub -all "::" [getFuncNameRaw $index] ""]
}

proc HA_BACKEND::getSetterGetterArrayNameForFunc {funcName} {
  regsub -all "get" $funcName "" funcName
	regsub -all "set" $funcName "" funcName
	return "${funcName}_impl"
}

proc HA_BACKEND::setter_for_array {array_name key value} {
  global $array_name
	if {![info exists $array_name]} {
		array set $array_name {}
	}
	set ${array_name}($key) $value
}

proc HA_BACKEND::getter_for_array {array_name key {additionalOptions ""}} {
	global $array_name
	if {![info exists $array_name] || ![info exists ${array_name}($key)]} {
		if {[lsearch $additionalOptions -allow-no-exist] >= 0} {
			return ""
		}
		error "Cannot get value for key $key from $array_name; has not been set!"
  }
	return [set ${array_name}($key)]
}

proc HA_BACKEND::PatternMatch_create {} {
	global PatternMatch_count
	incr PatternMatch_count
	set match "match${PatternMatch_count}"
	return $match
}

proc HA_BACKEND::PatternMatch_set_Pattern {patternMatch pattern} {
  return [setter_for_array [getSetterGetterArrayNameForFunc [getFuncName]] "${patternMatch}" $pattern]
}

proc HA_BACKEND::PatternMatch_get_Pattern {patternMatch} {
  return [getter_for_array [getSetterGetterArrayNameForFunc [getFuncName]] "${patternMatch}"]
}

proc HA_BACKEND::PatternMatch_set_matching_node_in_cdfg {patternMatch patternNode cdfgNode} {
  return [setter_for_array [getSetterGetterArrayNameForFunc [getFuncName]] "${patternMatch}_${patternNode}" $cdfgNode]
}

proc HA_BACKEND::PatternMatch_get_matching_node_in_cdfg {patternMatch patternNode} {
  return [getter_for_array [getSetterGetterArrayNameForFunc [getFuncName]] "${patternMatch}_${patternNode}"]
}

proc HA_BACKEND::PatternMatch_get_bound_nodes {patternMatch} {
	set array_name [getSetterGetterArrayNameForFunc "HA_BACKENDPatternMatch_get_matching_node_in_cdfg"]
	global $array_name
	foreach potential [array names $array_name] {
		set patternNode ""
		regexp "${patternMatch}_\(.*\)" $potential -> patternNode
		if {$patternNode != "" && [PatternMatch_get_matching_node_in_cdfg $patternMatch $patternNode] != ""} {
			lappend ret $patternNode
		}
	}
	if {![info exists ret]} {
		error "Patternmatch finding $array_name : [array names $array_name] failed!"
	}
	return $ret
}

proc HA_BACKEND::PatternMatch_is_same_as_PatternMatch {patternMatch0 patternMatch1} {
	set pattern0 [PatternMatch_get_Pattern $patternMatch0]
	set pattern1 [PatternMatch_get_Pattern $patternMatch1]
	if {$pattern0 != $pattern1} {
		return 0
	}
  foreach node [Pattern_get_all_nodes $pattern0] {
		set bound0 [PatternMatch_get_matching_node_in_cdfg $patternMatch0 $node]
		set bound1 [PatternMatch_get_matching_node_in_cdfg $patternMatch1 $node]
		if {$bound0 != $bound1} {
			return 0
		}
	}
	return 1
}

proc HA_BACKEND::MetaOperation_begin {_cdfg _uniqueTag _node} {
  global HA_BACKEND::MetaOperation_cdfg
	set HA_BACKEND::MetaOperation_cdfg ${_cdfg}
	global HA_BACKEND::MetaOperation_uniqueTag
	set HA_BACKEND::MetaOperation_uniqueTag ${_uniqueTag}
	global HA_BACKEND::MetaOperation_node
	set HA_BACKEND::MetaOperation_node ${_node}
}

proc HA_BACKEND::MetaOperation_create_operation {opName paramList} {
	global HA_BACKEND::MetaOperation_cdfg
	set cdfg $HA_BACKEND::MetaOperation_cdfg
	global HA_BACKEND::MetaOperation_uniqueTag
	set uniqueTag $HA_BACKEND::MetaOperation_uniqueTag
	set opName "${opName}_${uniqueTag}"
	create_operation $cdfg $opName
	set port 0
  foreach paramTraits $paramList {
		set param [lindex $paramTraits 0]
		set type  [lindex $paramTraits 1]
		if {[llength $paramTraits] > 2} {
		  set width [lindex $paramTraits 2]
		  MetaOperation_create_operand $param $width
		}
		if {$type == "in"} {
		  operation_set_src_operand $cdfg $opName $param
			operation_set_src_pin_attribute $cdfg $opName $param port $port
		} else {
			operation_set_dst_operand $cdfg $opName $param
			operation_set_dst_pin_attribute $cdfg $opName $param port $port
		}
		incr port
	}
	#copy attributes from saved node
  global HA_BACKEND::MetaOperation_node
	set node $HA_BACKEND::MetaOperation_node
	foreach attrib {color state cseq context originName c_op} {
	  operation_set_attribute $cdfg $opName $attrib [operation_get_attribute $cdfg $node $attrib]
	}
	return $opName
}

proc HA_BACKEND::MetaOperation_create_operand {opName width} {
	global HA_BACKEND::MetaOperation_cdfg
	set cdfg $HA_BACKEND::MetaOperation_cdfg
	global HA_BACKEND::MetaOperation_uniqueTag
	set uniqueTag $HA_BACKEND::MetaOperation_uniqueTag
	set opName "${opName}_${uniqueTag}"
	create_operand $cdfg $opName
	operand_set_attribute $cdfg $opName width $width
	return $opName
}

proc HA_BACKEND::MetaOperation_create_const {value {width -1}} {
	set value [expr $value]
	if {$value < 0} {
		set value [expr -$value]
		set ret "constn_$value"
	} else {
		set ret "constp_$value"
	}
	if {$width <= 0} {
		set width [expr int(ceil(log($value+2)/log(2)))]
	}
	return [HA_BACKEND::MetaOperation_create_operand $ret $width]
}

proc HA_BACKEND::MetaOperation_end {} {
}

proc HA_BACKEND::MetaOperation_detach {operation0 operand0} {
	global HA_BACKEND::MetaOperation_cdfg
	set cdfg $HA_BACKEND::MetaOperation_cdfg
	set dummyVal [getDummyValue]
	create_operand $cdfg $dummyVal
	operand_copy_attributes_from_operand $cdfg $dummyVal $operand0
	operation_replace_dst_operand_with_operand $cdfg $operation0 $operand0 $dummyVal
}

proc HA_BACKEND::set_match_replacement_creation_apis {slaveInterp cdfg patternMatch uniqueTag} {
	set_requirement_checking_apis $slaveInterp $cdfg
	interp alias $slaveInterp MetaOperation::begin "" HA_BACKEND::MetaOperation_begin $cdfg $uniqueTag
	interp alias $slaveInterp MetaOperation::create_operation "" HA_BACKEND::MetaOperation_create_operation
	interp alias $slaveInterp MetaOperation::create_operand "" HA_BACKEND::MetaOperation_create_operand
	interp alias $slaveInterp MetaOperation::create_const "" HA_BACKEND::MetaOperation_create_const
	interp alias $slaveInterp MetaOperation::end "" HA_BACKEND::MetaOperation_end
	interp alias $slaveInterp MetaOperation::detach "" HA_BACKEND::MetaOperation_detach
	$slaveInterp eval "proc GetBoundNode {node} {array set boundNodes {[HA_BACKEND::create_matchset_from_PatternMatch $patternMatch]}; return \$boundNodes(\$node)}"
}

proc HA_BACKEND::create_match_replacement {cdfg patternMatch uniqueTag} {
	set pattern [PatternMatch_get_Pattern $patternMatch]
	set locSlave [load_pattern_file [Pattern_get_pattern_file $pattern]]
	set_match_replacement_creation_apis $locSlave $cdfg $patternMatch $uniqueTag
	puts "Creating match replacement for [HA_BACKEND::create_matchset_from_PatternMatch $patternMatch] on pattern [Pattern_get_pattern_file $pattern]"
	set command "Create_Match_Replacement $cdfg $uniqueTag"
	foreach inputOperand [Pattern_get_input_interface $pattern] {
		lappend command [PatternMatch_get_matching_node_in_cdfg $patternMatch $inputOperand]
	}
	foreach outputOperand [Pattern_get_output_interface $pattern] {
		lappend command [PatternMatch_get_matching_node_in_cdfg $patternMatch $outputOperand]
	}
  set retcdfg [$locSlave eval $command]
	interp delete $locSlave
	return $retcdfg
}

proc HA_BACKEND::Pattern_create {} {
  package require struct::graph
	set pattern [::struct::graph]
	$pattern set type "Pattern"
	return $pattern
}

proc HA_BACKEND::Pattern_set_optional_attribute {pattern attribute value} {
  return [setter_for_array [getSetterGetterArrayNameForFunc [getFuncName]] "${pattern}_${attribute}" $value]
}

proc HA_BACKEND::Pattern_get_optional_attribute {pattern attribute} {
	return [getter_for_array [getSetterGetterArrayNameForFunc [getFuncName]] "${pattern}_${attribute}" -allow-no-exist]
}

#TODO: change to not be the pattern file itself, but be the contents of the pattern file
proc HA_BACKEND::Pattern_set_pattern_file {pattern patternFile} {
	$pattern set patternFile $patternFile
}

#TODO: see above
proc HA_BACKEND::Pattern_get_pattern_file {pattern} {
	return [$pattern get patternFile]
}

proc HA_BACKEND::Pattern_set_input_interface {pattern inputInterface} {
	$pattern set input_interface $inputInterface
	foreach input $inputInterface {
		$pattern node insert $input
    $pattern node set $input type "operand"
		$pattern node set $input interfaceType input
	}
}

proc HA_BACKEND::Pattern_get_input_interface {pattern} {
	return [$pattern get input_interface]
}

proc HA_BACKEND::Pattern_set_output_interface {pattern outputInterface} {
	$pattern set output_interface $outputInterface
	foreach output $outputInterface {
		$pattern node insert $output
		$pattern node set $output type "operand"
    $pattern node set $output interfaceType output
	}
}

proc HA_BACKEND::Pattern_get_output_interface {pattern} {
	return [$pattern get output_interface]
}

proc HA_BACKEND::Pattern_get_all_nodes {pattern} {
	return [$pattern nodes]
}

proc HA_BACKEND::Pattern_set_name {pattern patternName} {
	$pattern set patternName $patternName
}

proc HA_BACKEND::Pattern_get_name {pattern} {
	return [$pattern get patternName]
}

proc HA_BACKEND::Pattern_add_component {pattern nodeName inputList outputList} {
	set nodeName [$pattern node insert $nodeName]
	$pattern node set $nodeName type "operation"
	set pinNum 0
	foreach input $inputList {
		if {![$pattern node exists $input]} {
			$pattern node insert $input
			$pattern node set $input type "operand"
		}
		set incomingArc [$pattern arc insert $input $nodeName]
		$pattern arc set $incomingArc pinNum $pinNum
		incr pinNum
	}
	set pinNum 0
	foreach output $outputList {
		if {![$pattern node exists $output]} {
			$pattern node insert $output
			$pattern node set $output type "operand"
		}
		set outgoingArc [$pattern arc insert $nodeName $output]
		$pattern arc set $outgoingArc pinNum $pinNum
		incr pinNum
	}
}

proc HA_BACKEND::Pattern_set_pin_from_to {pattern component operand pinNum} {
	if {$pinNum == "-any"} {
		set pinNum ""
	}
  set_pin_number_for_arc_from $pattern $component $operand $pinNum
}

proc HA_BACKEND::Pattern_set_element_regex {pattern element regex} {
  $pattern node set $element regex $regex
}

proc HA_BACKEND::Pattern_get_element_regex {pattern element} {
	if {[$pattern node keyexists regex]} {
		return [$pattern node keyexists regex]
	}
	return ""
}

proc HA_BACKEND::Pattern_add_requirement {pattern opList requirement} {
  $pattern lappend requires [list $opList $requirement]
}

proc HA_BACKEND::Pattern_get_requirements {pattern} {
	if {[$pattern keyexists requires]} {
	  return [$pattern get requires]
	}
	return ""
}

proc HA_BACKEND::Pattern_set_visit_order {pattern order} {
	$pattern set visitOrder $order
}

#TODO: extend to include requirements
#TODO: what is this?
proc HA_BACKEND::get_requirement_walk_order {graph node_walk_order} {
}

#given an existing set of nodes, if we added the new node addedNode, what new requirements from the list get satisfied?
proc HA_BACKEND::get_requirements_satisfied_by_adding_node {pattern requirements existingNodes addedNode} {
  set satisfiedRequirements {}
	foreach requirementPair $requirements {
		set isSatisfied 1
		set usesAddedNode 0
		foreach input [lindex $requirementPair 0] {
			if {$input == $addedNode} {
				set usesAddedNode 1
			} elseif {[lsearch $existingNodes $input] == -1} {
				set isSatisfied 0
			}
		}
		if {$isSatisfied && $usesAddedNode} {
			lappend satisfiedRequirements $requirementPair
		}
	}
  return $satisfiedRequirements
}

#TODO: implement
#how common are the requirements in the graph? (ie, how often does a random sampling satisfy them?)
proc HA_BACKEND::get_rarity_of_requirements {cdfg pattern requirements} {
	set cdfgSize 1000
  array set weight {}
	array set regexNum {}
	foreach requirement $requirements {
		set cmd [lindex $requirement 1]
		if {[regexp {return \[.* consumes .*\]} $requirement] ||
			  [regexp {return \[.* produces .*\]} $requirement]} {
			set weight($requirement) 0.5
		} elseif {[regexp {return \[has (\d*) inputs (\d*) outputs\]} $requirement -> numInput numOutputs]} {
			set weight($requirement) 0.1
		} elseif {[regexp {return \[regexp \{(.*)\} (.*)\]} $requirement -> regex node]} {
      #if we've already calculated it, dont recalc
			if {[info exists regexNum($regex)]} {
        set weight($requirement) $regexNum($regex)
			} elseif {[$pattern node get $node type] == "operand"} {
				set count 0
			  for_each_operand op $cdfg {
					if {[regexp $regex $op]} {
						incr count
					}
				}
			} elseif {$pattern node get $node type] == "operation"} {
				set count 0
				for_each_operation op $cdfg {
					if {[regexp $regex $op]} {
						incr count
					}
				}
			} else {
			}
		} else {
      #evaluate?
		  set weight($requirement) 0.001
		}
	}
	return [array get weight]
}

proc HA_BACKEND::get_all_requirements {pattern} {
#get the explicit pattern level requirements
	set ret [Pattern_get_requirements $pattern]
#get the explicit node regexes
	foreach node [Pattern_get_all_nodes $pattern] {
		if {[$pattern node keyexists $node regex]} {
  		set regex [$pattern node get $node regex]
			set regexCmd "return \[regexp {$regex} $node\]"
      lappend ret [list $node $regexCmd]
		}
  }
#get the connectivity requirements (pin in-out/consumption-production number of operations/operands)
	foreach node [Pattern_get_all_nodes $pattern] {
		set inputs  [$pattern nodes -in $node]
		set outputs [$pattern nodes -out $node]
		lappend ret [list $node "return \[has [llength $inputs] inputs [llength $outputs] outputs\]"]
	}
#get the connections between pattern nodes as requirements
	foreach arc [$pattern arcs] {
		set arcSource [$pattern arc source $arc]
		set arcTarget [$pattern arc target $arc]
		set cmd ""
		if {[$pattern node get $arcSource type] == "operand"} {
      set cmd "\[$arcTarget consumes $arcSource\]"
		}
		if {[$pattern node get $arcTarget type] == "operand"} {
			set cmd "\[$arcSource produces $arcTarget\]"
		}
		lappend ret [list [list $arcSource $arcTarget] "return $cmd"]
	}
	return $ret
}

#TODO: need to pass the requirement list and weights as parameters, not recalc every call
proc HA_BACKEND::get_best_node {cdfg pattern mappedNodes unmappedNodes} {
	set requirements [get_all_requirements $pattern]
	array set requirementWeight [get_rarity_of_requirements $cdfg $pattern $requirements]
  #get the weights of the requirements satisfied by each node
	array set nodeWeights {}
	foreach node $unmappedNodes {
		set nodeWeights($node) ""
		set satisfied [get_requirements_satisfied_by_adding_node $pattern $requirements $mappedNodes $node]
		foreach satisfiedReq $satisfied {
      lappend nodeWeights($node) $requirementWeight($satisfiedReq)
		}
	}
  #multiply the probabilities to find the minimum (?) weight
	array set totalWeight {}
	foreach node $unmappedNodes {
		set totalWeight($node) 1
		foreach weight $nodeWeights($node) {
			set totalWeight($node) [expr $totalWeight($node) * $weight]
		}
	}
  #find the minimum probability, that should be the best (ie, rarest)
	set minNode [lindex $unmappedNodes 0]
	set minWeight $totalWeight($minNode)
	foreach node $unmappedNodes {
		set weight $totalWeight($node)
		if {$weight < $minWeight} {
			set minNode $node
			set minWeight $weight
		}
	}
	return $minNode
}

#TODO: we really want a hueristic to decide a walk ordering. All walk orderings will give valid results, but they will result in differing run times. Examples:
#    if one regex maps to very few nodes in the cdfg, it is better to walk it early
#    if one pattern node has very high connectivity, and the cdfg has very few nodes with that high of connectivity, it is better to walk that node early
#    if we can apply pattern-level regex's earlier in our walk, it is better to walk those nodes early so as to reject bad matches
#these are some examples of hueristics that we should be applying in order to do a good walk.
proc HA_BACKEND::get_connected_component_walk_order {graph nodeList {startingNode ""} {needsDelete 1} {directionList "start"}} {
	if {$startingNode == ""} {
		set startingNode [lindex $nodeList 0]
	}
	set walkOrder {}
	upvar isWalked isWalked
	if {![info exists isWalked]} {
		set isWalked ""
	}
	if {[lsearch $isWalked $startingNode] == -1} {
	#if we havent processed this node, process it!
		lappend isWalked $startingNode
		lappend walkOrder [list $startingNode $directionList]
    #get all adjacent nodes
		set adjNodes [$graph nodes -adj $startingNode]
		foreach adj $adjNodes {
      #create the new directionList
			set adjDirectionList {}
			foreach walkedNode $isWalked {
				set prevAdjNodes [$graph nodes -in $walkedNode]
				if {[lsearch $prevAdjNodes $adj] != -1} {
				  set pinNum [get_pin_number_for_arc_from $graph $adj $walkedNode]
					if {$pinNum != ""} {
				    lappend adjDirectionList [list in $walkedNode $pinNum]
					} else {
						lappend adjDirectionList [list in $walkedNode]
					}
				}
				set prevAdjNodes [$graph nodes -out $walkedNode]
				if {[lsearch $prevAdjNodes $adj] != -1} {
				  set pinNum [get_pin_number_for_arc_from $graph $walkedNode $adj]
					if {$pinNum != ""} {
				    lappend adjDirectionList [list out $walkedNode $pinNum]
					} else {
						lappend adjDirectionList [list out $walkedNode]
					}
				}
			}
      #call the child
			set childWalkOrder [get_connected_component_walk_order $graph $nodeList $adj 0 $adjDirectionList]
			foreach element $childWalkOrder {
				lappend walkOrder $element
			}
		}
  }
	if {$needsDelete} {
		unset isWalked
	}
	return $walkOrder
}

#TODO: figure out why calling isConnected is erroring 'invalid command name'
proc HA_BACKEND::Pattern_get_visit_order {pattern} {
	if {[$pattern keyexists visitOrder]} {
		return [$pattern get visitOrder]
	}
	#if {1 && [::struct::graph::op::isConnected $pattern]} {}
	if {1} {
		set firstOperation [lindex [$pattern nodes -key type -value "operation"] 0]
    #TODO: this doesnt have to specify a first operation
		set patternNodes [get_connected_component_walk_order $pattern [$pattern nodes] $firstOperation]
	} else {
	  set connectedComponents [::struct::graph::op::connectedComponents $pattern]
	  foreach componentSet $connectedComponents {
		  set walkOrder [get_connected_component_walk_order $pattern $componentSet]
			foreach element $walkOrder {
				lappend patternNodes $element
			}
	  }
	}
	#HA_BACKEND::Pattern_set_visit_order $pattern $patternNodes
	return $patternNodes
}

proc HA_BACKEND::Temp_Pattern_begin {{patternName ""}} {
	global curPattern
	set curPattern [Pattern_create]
	Pattern_set_name $curPattern $patternName
}
proc HA_BACKEND::Temp_Pattern_attributes {attributes} {
	global curPattern
	foreach attribute $attributes {
    #TODO: allow attributes to have specific values set
    Pattern_set_optional_attribute $curPattern $attribute 1
	}
}
proc HA_BACKEND::Temp_Pattern_interface {inputs outputs} {
	global curPattern
	Pattern_set_input_interface $curPattern $inputs
	Pattern_set_output_interface $curPattern $outputs
}
proc HA_BACKEND::Temp_Pattern_component {operatorName inputs outputs} {
	global curPattern
  Pattern_add_component $curPattern $operatorName $inputs $outputs
}
proc HA_BACKEND::Temp_Pattern_element_regex {element regex} {
  global curPattern
	Pattern_set_element_regex $curPattern $element $regex
}
proc HA_BACKEND::Temp_Pattern_pin_from_to {component operand pinNum} {
	global curPattern
	Pattern_set_pin_from_to $curPattern $component $operand $pinNum
}
proc HA_BACKEND::Temp_Pattern_requires {input requirement} {
	global curPattern
	Pattern_add_requirement $curPattern $input $requirement
}
proc HA_BACKEND::Temp_Pattern_visit_order {order} {
	global curPattern
	Pattern_set_visit_order $curPattern $order
}
proc HA_BACKEND::Temp_Pattern_end {} {
}

#TODO: what about being able to say a and b are in a cone?
proc HA_BACKEND::set_Pattern_creation_apis {locSlave} {
	interp alias $locSlave Pattern::begin "" HA_BACKEND::Temp_Pattern_begin
	interp alias $locSlave Pattern::attributes "" HA_BACKEND::Temp_Pattern_attributes
	interp alias $locSlave Pattern::interface "" HA_BACKEND::Temp_Pattern_interface
	interp alias $locSlave Pattern::component "" HA_BACKEND::Temp_Pattern_component
	interp alias $locSlave Pattern::element_regex "" HA_BACKEND::Temp_Pattern_element_regex
	interp alias $locSlave Pattern::pin_from_to "" HA_BACKEND::Temp_Pattern_pin_from_to
	interp alias $locSlave Pattern::requires "" HA_BACKEND::Temp_Pattern_requires
	interp alias $locSlave Pattern::visit_order "" HA_BACKEND::Temp_Pattern_visit_order
	interp alias $locSlave Pattern::end "" HA_BACKEND::Temp_Pattern_end
}

#TODO: change these to use the arg binding syntax
proc HA_BACKEND::get_pattern_from_file {fileName} {
	set locSlave [load_pattern_file $fileName]
  #register functions
	set_Pattern_creation_apis $locSlave
  #execute
	$locSlave eval "Create_Pattern"
	global curPattern
	Pattern_set_pattern_file $curPattern $fileName
	interp delete $locSlave
	return $curPattern
}

proc HA_BACKEND::get_pattern_from_block {block} {
	set locSlave [interp create]
  #register functions
  set_Pattern_creation_apis $locSlave
  #execute
  $locSlave eval $block
	global curPattern
	#NOTE: patternfile not set, so this cannot be used for replacements!
	interp delete $locSlave
	return $curPattern
}

proc HA_BACKEND::create_graph_from_cdfg {cdfg} {
  package require struct::graph
  set graph [::struct::graph]
	$graph set type "CDFG"
  for_each_operand oper $cdfg {
		$graph node insert $oper
		$graph node set $oper type "operand"
	}
	for_each_operation oper $cdfg {
		$graph node insert $oper
		$graph node set $oper type "operation"
		$graph node set $oper state [operation_get_attribute $cdfg $oper state]
		for_each_operation_src src $cdfg oper {
			set incomingArc [$graph arc insert $src $oper]
			$graph arc set $incomingArc pinNum [operation_get_src_pin_attribute $cdfg $oper $src position]
		}
		for_each_operation_dst dst $cdfg oper {
			set outgoingArc [$graph arc insert $oper $dst]
			$graph arc set $outgoingArc pinNum [operation_get_dst_pin_attribute $cdfg $oper $dst position]
		}
	}
	return $graph
}

#matchset is a list of pairs of elements mapped to each other, so for example:
#{a 0 b 3 c 2 out 10}
#maps a to 0, b to 3, c to 2, and out to 10
proc HA_BACKEND::create_PatternMatch_from_matchset {matchset pattern} {
	set match [PatternMatch_create]
  PatternMatch_set_Pattern $match $pattern
	foreach {key value} $matchset {
		PatternMatch_set_matching_node_in_cdfg $match $key $value
	}
	return $match
}

proc HA_BACKEND::create_matchset_from_PatternMatch {patternMatch} {
	set matchset {}
	foreach key [PatternMatch_get_bound_nodes $patternMatch] {
		lappend matchset $key
		lappend matchset [PatternMatch_get_matching_node_in_cdfg $patternMatch $key]
	}
	return $matchset
}

proc HA_BACKEND::lremove_value {listName value} {
	set idx [lsearch $listName $value]
	return [lreplace $listName $idx $idx]
}

proc HA_BACKEND::list_intersect {list0 list1} {
	set smaller $list0
	set larger  $list1
	if {[llength $list0] > [llength $list1]} {
	  set smaller $list1
		set larger  $list0
	}
	set ret ""
	foreach element $smaller {
		if {[lsearch -exact $larger $element] != -1} {
			lappend ret $element
		}
	}
	return $ret
}

proc HA_BACKEND::get_arcs_from {graph start finish} {
	set startArcs [$graph arcs -out $start]
	set finishArcs [$graph arcs -in $finish]
	return [list_intersect $startArcs $finishArcs]
}

proc HA_BACKEND::set_pin_number_for_arc_from {graph start finish pinNum} {
	set arcs [get_arcs_from $graph $start $finish]
	set arc [lindex $arcs 0]
	if {$arc == ""} {
		error "Cannot set pin number for arc from $start to $finish in $graph; arc does not exist!"
	}
	$graph arc set $arc pinNum $pinNum
}

proc HA_BACKEND::get_pin_number_for_arc_from {graph start finish} {
	set arcs [get_arcs_from $graph $start $finish]
	set arc [lindex $arcs 0]
	return [$graph arc get $arc pinNum]
}

proc HA_BACKEND::filter_nodes_by_unique {pattern curMatchSet curNode filtered} {
	set ret {}
	foreach potential $filtered {
		if {[lsearch $curMatchSet $potential] == -1} {
			lappend ret $potential
		}
	}
	return $ret
}

proc HA_BACKEND::filter_nodes_by_regex {pattern curNode filtered} {
	if {[$pattern node keyexists $curNode regex]} {
		set regex [$pattern node get $curNode regex]
		foreach potential $filtered {
			if {![regexp $regex $potential]} {
				set filtered [lremove_value $filtered $potential]
			}
		}
	} 
  return $filtered
}

proc HA_BACKEND::requirement_helper_GetWidth {cdfg operand} {
	return [operand_get_attribute $cdfg $operand width]
}

proc HA_BACKEND::requirement_helper_GetConstValue {operand} {
	if {[regexp {^constp_(\d*)} $operand -> val]} {
		return $val
	}
	if {[regexp {^constn_(\d*)} $operand -> val]} {
		return [expr -$val]
	}
	if {[regexp {^1b(\d*)} $operand -> val]} {
		return $val
	}
	return ""
}

proc HA_BACKEND::set_requirement_checking_apis {slaveInterp cdfg} {
	interp alias $slaveInterp GetWidth "" HA_BACKEND::requirement_helper_GetWidth $cdfg
	interp alias $slaveInterp GetConstValue "" HA_BACKEND::requirement_helper_GetConstValue
}

proc HA_BACKEND::create_requirement_checking_interp {pattern cdfg} {
	set requiresFullFileLoad [Pattern_get_optional_attribute $pattern "-requires-full-file-load"]
	if {$requiresFullFileLoad == 1} {
		set slave [load_pattern_file [Pattern_get_pattern_file $pattern]]
	} else {
	  set slave [interp create]
	}
	set_requirement_checking_apis $slave $cdfg
  return $slave
}

proc HA_BACKEND::filter_nodes_by_requirements {cdfg pattern curMatchSet curNode filtered} {
	array set matchArray $curMatchSet
  #TODO: make sure that past checked requirements are not rechecked, only immediately satisfied ones
  set satisfiedRequirements {}
	foreach requirementPair [Pattern_get_requirements $pattern] {
		set isSatisfied 1
		foreach input [lindex $requirementPair 0] {
			if {![info exists matchArray($input)] && $input != $curNode} {
				set isSatisfied 0
			}
		}
		if {$isSatisfied} {
			lappend satisfiedRequirements $requirementPair
		}
	}
	foreach requirementPair $satisfiedRequirements {
		set inputList		[lindex $requirementPair 0]
		set requirement [lindex $requirementPair 1]
    set slave [create_requirement_checking_interp $pattern $cdfg]
    #set the already locked in nodes
		foreach input $inputList {
			if {$input != $curNode} {
		    $slave eval "set $input $matchArray($input)"
			}
		}
    #test possible curnodes
		foreach potential $filtered {
		  $slave eval "set $curNode $potential"
			set pass [$slave eval $requirement]
			if {!$pass} {
				set filtered [lremove_value $filtered $potential]
			}
		}
	  interp delete $slave
	}
	return $filtered
}

proc HA_BACKEND::filter_nodes_by_already_made_connection {pattern cdfgGraph curMatchSet curDirList filtered} {
	array set matchArray $curMatchSet
	foreach dirPair $curDirList {
		set direction [lindex $dirPair 0]
		if {$direction == "start"} {
			continue
		}
		set adjNode   [lindex $dirPair 1]
		set cdfgAdjNode $matchArray($adjNode)
		set adjSet [$cdfgGraph nodes -$direction $cdfgAdjNode]
		set pinNum    [lindex $dirPair 2]
		if {$pinNum != ""} {
			set actualAdjSet {}
			foreach realPotential $adjSet {
				if {$direction == "in"} {
			    set cdfgPinNum [get_pin_number_for_arc_from $cdfgGraph $realPotential $cdfgAdjNode]
				} else {
			    set cdfgPinNum [get_pin_number_for_arc_from $cdfgGraph $cdfgAdjNode $realPotential]
				}
				if {$cdfgPinNum == $pinNum} {
					lappend actualAdjSet $realPotential
				}
			}
			set adjSet $actualAdjSet
		}
		set filtered [list_intersect $filtered $adjSet]
	}
	return $filtered
}

proc HA_BACKEND::filter_nodes_by_maximum_connection {pattern cdfgGraph curNode filtered} {
	set requiredIncoming [llength [$pattern nodes -in $curNode]]
	set requiredOutgoing [llength [$pattern nodes -out $curNode]]
	set ret {}
	foreach potentialNode $filtered {
		set potentialIncoming [llength [$cdfgGraph nodes -in $potentialNode]]
		if {$requiredIncoming > $potentialIncoming} {
			continue
		}
		set potentialOutgoing [llength [$cdfgGraph nodes -out $potentialNode]]
		if {$requiredOutgoing > $potentialOutgoing} {
			continue
		}
		lappend ret $potentialNode
	}
	return $ret
}

#TODO: optimize by operating directly on the cdfg, rather than through a cdfgGraph
#return some list of nodes in cdfg that match curNode in pattern, given the curPatternMatch that is already mapped
proc HA_BACKEND::get_filtered_nodes {cdfg pattern cdfgGraph curMatchSet curNode {curDirList "start"}} {
	#puts "starting get_filtered_nodes $pattern $cdfgGraph {$curMatchSet} $curNode $curDirList"
	array set matchArray $curMatchSet
  if {$curDirList == "start"} {
    #start with all the nodes of the correct type
	  set filtered [$cdfgGraph nodes -key type -value [$pattern node get $curNode type]]
	} else {
		set firstDirPair [lindex $curDirList 0]
		set filtered [$cdfgGraph nodes -[lindex $firstDirPair 0] $matchArray([lindex $firstDirPair 1])]
	}
  #filter out those nodes which are already mapped
	set filtered [filter_nodes_by_unique $pattern $curMatchSet $curNode $filtered]
  #filter out those nodes which dont even have enough connections for our pattern
	set filtered [filter_nodes_by_maximum_connection $pattern $cdfgGraph $curNode $filtered]
  #filter out those nodes that arent connected correctly already
  set filtered [filter_nodes_by_already_made_connection $pattern $cdfgGraph $curMatchSet $curDirList $filtered]
  #filter out those nodes that don't satisfy regex
  set filtered [filter_nodes_by_regex $pattern $curNode $filtered]
  #filter out those nodes that don't satisfy requires of current node
  set filtered [filter_nodes_by_requirements $cdfg $pattern $curMatchSet $curNode $filtered]
	#puts "found $filtered"
  return $filtered
}

proc HA_BACKEND::get_all_matchsets_of_Pattern_in_cdfg_given {cdfg pattern cdfgGraph curMatchSet curNodeIndex patternNodes} {
  #if we are done processing, just return the match
	if {$curNodeIndex >= [llength $patternNodes]} {
		return [list $curMatchSet]
	}
  #if we are not done processing, process this node
	set curNode [lindex [lindex $patternNodes $curNodeIndex] 0]
	set curDirList [lindex [lindex $patternNodes $curNodeIndex] 1]
	set filtered_nodes [get_filtered_nodes $cdfg $pattern $cdfgGraph $curMatchSet $curNode $curDirList]
	set matches {}
	foreach node $filtered_nodes {
    #bind the current node to its position
		set newMatch $curMatchSet
		lappend newMatch $curNode
		lappend newMatch $node
		set result [get_all_matchsets_of_Pattern_in_cdfg_given $cdfg $pattern $cdfgGraph $newMatch [expr $curNodeIndex+1] $patternNodes]
		foreach matchset $result {
			lappend matches $matchset
		}
	}
	return $matches
}

proc HA_BACKEND::get_all_PatternMatch_of_Pattern_in_graph {cdfg pattern cdfgAsGraph} {
  #order patternNodes
  set patternNodes [Pattern_get_visit_order $pattern]
	set allMatchSets [get_all_matchsets_of_Pattern_in_cdfg_given $cdfg $pattern $cdfgAsGraph {} 0 $patternNodes]
  set allPatternMatches {}
	foreach matchSet $allMatchSets {
		array set matchArray $matchSet
    #check that the requires of this Pattern are being honored
		set validPattern 1
    foreach requirementPair [Pattern_get_requirements $pattern] {
			set input [lindex $requirementPair 0]
      set requirement [lindex $requirementPair 1]
			set slave [create_requirement_checking_interp $pattern $cdfg]
			foreach key $input {
				set value $matchArray($key)
			  $slave eval "set $key $value"
		  }
			set pass [$slave eval $requirement]
			if {!$pass} {
				set validPattern 0
			}
		  interp delete $slave
		}
		if {$validPattern} {
		  lappend allPatternMatches [create_PatternMatch_from_matchset $matchSet $pattern]
		}
	}
	return $allPatternMatches
}

proc HA_BACKEND::get_matched_nodes_in_cdfg_for_PatternMatch_calculate {patternMatch} {
  set ret {}
	foreach key [PatternMatch_get_bound_nodes $patternMatch] {
		lappend ret [PatternMatch_get_matching_node_in_cdfg $patternMatch $key]
	}
	return $ret
}

proc HA_BACKEND::get_matched_nodes_in_cdfg_for_PatternMatch {patternMatch} {
	set cache "cache_[getFuncName]"
	global $cache
	if {![info exists $cache]} {
    array set $cache {}
	}
	if {[info exists ${cache}($patternMatch)]} {
		return [set ${cache}($patternMatch)]
	}
	set ret [get_matched_nodes_in_cdfg_for_PatternMatch_calculate $patternMatch]
	set ${cache}($patternMatch) $ret
	return $ret
}

proc HA_BACKEND::do_PatternMatch_exclude_each_other {PatternMatch0 PatternMatch1} {
  set nodes0 [get_matched_nodes_in_cdfg_for_PatternMatch $PatternMatch0]
  set nodes1 [get_matched_nodes_in_cdfg_for_PatternMatch $PatternMatch1]
  foreach node $nodes0 {
		if {$node in $nodes1} {
			return 1
		}
	}
	return 0
}

proc HA_BACKEND::create_match_exclusion_graph {PatternMatchSet} {
  package require struct::graph
  set graph [struct::graph]
	foreach patternMatch $PatternMatchSet {
		$graph node insert $patternMatch
    foreach existingMatch [$graph nodes] {
			if {$patternMatch == $existingMatch} {
				continue
			}
      if {[do_PatternMatch_exclude_each_other $patternMatch $existingMatch]} {
				$graph arc insert $patternMatch $existingMatch
			}
		}
	}
	return $graph
}

proc HA_BACKEND::welsh_powell_greedy_coloring {graph} {
  #find degrees
	array set degree {}
	foreach node [$graph nodes] {
		set deg [llength [$graph nodes -adj $node]]
		if {![info exists degree($deg)]} {
			set degree($deg) {}
		}
		lappend degree($deg) $node
	}
  #order nodes by degrees
	set nodeOrder {}
	foreach deg [lsort -integer -increasing [array names degree]] {
		foreach node $degree($deg) {
			lappend nodeOrder $node
		}
	}
  #perform greedy coloring
	foreach node $nodeOrder {
		for {set color 0} {1} {incr color} {
      #see if we can use this color
			if {[llength [$graph nodes -adj $node -key color -value $color]] == 0} {
				$graph node set $node color $color
				break
			}
		}
	}
	return $graph
}

proc HA_BACKEND::max_color {graph} {
	for {set color 0} {[$graph nodes -key color -value $color] != ""} {incr color} {
	}
	return $color
}

proc HA_BACKEND::getDummyValueRegex {} {
	return "uniquify_"
}

proc HA_BACKEND::getDummyValue {{dummyValueCounterName "HA_BACKEND::dummyValueCounter"}} {
	global $dummyValueCounterName
	return "[getDummyValueRegex]_dummyValue_[incr $dummyValueCounterName]"
}

#checks if operation has no dst operands (other than dummyValue), and if so, removes it and calls recursive_remove on all src operands
#returns whether delete occured
proc HA_BACKEND::recursive_remove_operation {cdfg operation} {
	set all_src [cdfg_operation_get_all_src $cdfg $operation]
	set all_dst [cdfg_operation_get_all_dst $cdfg $operation]
	set hasRealDst 0
	for {set i 0} {$i < [$all_dst size]} {incr i} {
		set dst [$all_dst get $i]
		if {![regexp [getDummyValueRegex] $dst]} {
			set hasRealDst 1
		}
	}
	if {!$hasRealDst} {
		for {set i 0} {$i < [$all_src size]} {incr i} {
			set src [$all_src get $i]
			operation_remove_src_operand $cdfg $operation $src
    }
	  for {set i 0} {$i < [$all_dst size]} {incr i} {
			set dst [$all_dst get $i]
			operation_remove_dest_operand $cdfg $operation $dst
		}
		cdfg_remove_operation $cdfg $operation
	  for {set i 0} {$i < [$all_src size]} {incr i} {
			set src [$all_src get $i]
		  recursive_remove_operand $cdfg $src
	  }
		return 1
	}
	return 0
}

#checks if operand has no consumers, and if so, removes it (by replacing with dummy value) and calls recursive_remove on all producers
#returns whether delete occured
proc HA_BACKEND::recursive_remove_operand {cdfg operand} {
	set all_prod [cdfg_operand_get_all_producers $cdfg $operand]
	if {[[cdfg_operand_get_all_consumers $cdfg $operand] size] == 0} {
		for {set i 0} {$i < [$all_prod size]} {incr i} {
			set prod [$all_prod get $i]
			set dummyVal [getDummyValue]
			create_operand $cdfg $dummyVal
			operand_copy_attributes_from_operand $cdfg $dummyVal $operand
			operation_replace_dst_operand_with_operand $cdfg $prod $operand $dummyVal
		}
		cdfg_remove_operand $cdfg $operand
		for {set i 0} {$i < [$all_prod size]} {incr i} {
			set prod [$all_prod get $i]
			recursive_remove_operation $cdfg $prod
		}
		return 1
	}
	return 0
}

#TODO: add in code to remove all edges, except those marked as -save
proc HA_BACKEND::perform_match_replacement {cdfg patternMatch uniqueTag} {
	#remove the existing connections in the cdfg that we are replacing
  set pattern [PatternMatch_get_Pattern $patternMatch]
	set autoRemoveOff [Pattern_get_optional_attribute $pattern -auto-remove-off]
	set outputNodes {}
	foreach outputOperand [Pattern_get_output_interface $pattern] {
		lappend outputNodes [PatternMatch_get_matching_node_in_cdfg $patternMatch $outputOperand]
	}
	#un-attach the connections we found that are no longer valid - do this first so that,
	#   if we are just rearranging the order of operands or something, it wont misidentify
  #   a value it is supposed to unattach. If you are interested in reattaching, that is fine.
	set oldProducers {}
  foreach node $outputNodes {
		for_each_operand_producer prod $cdfg $node {
      if {$prod in [get_matched_nodes_in_cdfg_for_PatternMatch $patternMatch]} {
				if {$autoRemoveOff != 1} {
				  set dummyVal [getDummyValue]
				  create_operand $cdfg $dummyVal
				  operand_copy_attributes_from_operand $cdfg $dummyVal $node
				  operation_replace_dst_operand_with_operand $cdfg $prod $node $dummyVal
				}
				if {[lsearch $oldProducers $prod] == -1} {
				  lappend oldProducers $prod
				}
			}
		}
	}
  #then create the replacement
	create_match_replacement $cdfg $patternMatch $uniqueTag
  #then clean up any unattached values
	foreach oldProd $oldProducers {
    recursive_remove_operation $cdfg $oldProd
	}
}

proc HA_BACKEND::get_all_replaced_PatternMatches {cdfg} {
  return [getter_for_array [getSetterGetterArrayNameForFunc [getFuncName]] "${cdfg}" -allow-no-exist]
}

proc HA_BACKEND::set_all_replaced_PatternMatches {cdfg allPatternMatches} {
  return [setter_for_array [getSetterGetterArrayNameForFunc [getFuncName]] "${cdfg}" $allPatternMatches]
}

proc HA_BACKEND::should_ignore_PatternMatch {cdfg curPatternMatch allPatternMatches} {
	foreach existingMatch $allPatternMatches {
    if {[PatternMatch_is_same_as_PatternMatch $curPatternMatch $existingMatch]} {
			if {[Pattern_get_optional_attribute [PatternMatch_get_Pattern $existingMatch] -allow-revisit] != 1} {
			  return 1
			}
		}
	}
	return 0
}

proc HA_BACKEND::remove_zpad_from_graph {oldGraph} {
  package require struct::graph
  set graph [::struct::graph]
	$graph = $oldGraph
	set zpads ""
  #find all of the zpads
	foreach node [$graph nodes] {
		if {[regexp {^a2c_zpad} $node]} {
			lappend zpads $node
		}
	}
  #do the actual removal
	foreach zpad $zpads {
    set singleIn  [$graph nodes -in $zpad]
		set singleOut [$graph nodes -out $zpad]
		if {[llength $singleIn] != 1 || [llength $singleOut] != 1} {
      #if its not exactly 1 input 1 output, its not really a zpad
			continue
		}
    #replace all consumers of singleOut to actually consume singleIn
		foreach singleArc [$graph arcs -out $singleOut] {
      $graph arc move-source $singleArc $singleIn
		}
    #now we can remove the zpad as well as singleOut (not consumed anymore)
		$graph node delete $singleOut
		$graph node delete $zpad
	}
	return $graph
}

proc HA_BACKEND::find_replacement_max_set {cdfg allPatterns} {
	set graphCDFG [create_graph_from_cdfg $cdfg]
	set alreadyReplaced [get_all_replaced_PatternMatches $cdfg]
	set matches {}
	foreach pattern $allPatterns {
		if {[Pattern_get_optional_attribute $pattern -ignore-all-zpads] == 1} {
			if {![info exists removeZpadCDFG]} {
	      set removeZpadCDFG [remove_zpad_from_graph $graphCDFG]
			}
			set usedGraph $removeZpadCDFG
		} else {
			set usedGraph $graphCDFG
		}
	  foreach match [get_all_PatternMatch_of_Pattern_in_graph $cdfg $pattern $usedGraph] {
      if {![should_ignore_PatternMatch $cdfg $match $alreadyReplaced]} {
			  lappend matches $match
			}
    }
	}
  #color to find which matches can be replaced independant of each other
	set exclusion_set [create_match_exclusion_graph $matches]
  welsh_powell_greedy_coloring $exclusion_set
	set max_set {}
  for {set color 0} {$color < [max_color $exclusion_set]} {incr color} {
    set color_set [$exclusion_set nodes -key color -value $color]
		if {[llength $color_set] > [llength $max_set]} {
			set max_set $color_set
		}
	}
  return $max_set
}

proc HA_BACKEND::make_possible_replacements {cdfg allPatterns uniqueTag uniqueId} {
	set alreadyReplaced [get_all_replaced_PatternMatches $cdfg]
	set max_set [find_replacement_max_set $cdfg $allPatterns]
	foreach match $max_set {
		set tag "${uniqueTag}_$uniqueId"
		perform_match_replacement $cdfg $match $tag
		incr uniqueId
	}
  #update the replaced patternmatches
  foreach match $max_set {
		lappend alreadyReplaced $match
	}
	set_all_replaced_PatternMatches $cdfg $alreadyReplaced
  #return the number of matches made
	return [llength $max_set]
}

proc library_test {} {
	global cdfg
  global patterns

	set cdfg [get_cdfg_from_file ~/workspace/A2C/hw/regressions/HA_TRAN_PLUS_turboDeRateMatch/full_dump.tcl]
	puts [time {
		set patterns {}
		foreach patternFile [glob -directory ./constPatterns *.tcl] {
			lappend patterns [HA_BACKEND::get_pattern_from_file $patternFile]
		}
	  set uniqueId 0
	  for {set count 0} {1} {incr count} {
      set num_replacements [HA_BACKEND::make_possible_replacements $cdfg $patterns "replacement_" $uniqueId]
			incr uniqueId $num_replacements
			if {$num_replacements == 0} {
				break
			}
	  }
  }]
	puts "$uniqueId patterns replaced in $count match iterations."
}
