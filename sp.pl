#!/usr/bin/perl
use warnings;
use strict;

sub show_djka_header();
sub init_cost_matrix($);
sub show_node_entry();
sub show_cost_metric(\@);
sub extract_rect(\@$$$$);
sub get_node_name($);
sub has_node_inside($\@);
sub do_floyd(\@);
sub min($$);
sub do_dijkstra();
sub explore($);

my @cost_matrix;
my %node_ids;
my $init_file;
my $node_count = 0;


# START OF THE MAIN PROGRAM HERE ##
show_djka_header();
chomp($init_file = <STDIN>);
init_cost_matrix($init_file) or die "Syntax error in init file!\n";
show_node_entry();
show_cost_metric(@cost_matrix);
do_dijkstra();

# do_floyd (@cost_matrix);
# show_cost_metric(@cost_matrix);

# END OF THE MAIN PROGRAM HERE   ##
# my @rect = @{extract_rect(@cost_matrix, 2, 4, 2, 5)};
# show_cost_metric(@rect);


## START OF DEFINITION OF THE SUBROUTINES ##

sub show_djka_header()
{
	print "------------------------------------------------------*-\n";
	print "--                                                    *-\n";
	print "--                  Dijkstra Algorithm                *-\n";
	print "--                                                    *-\n";
	print "------------------------------------------------------*-\n";
	print "\nEnter the file name ->   ";
}

sub init_cost_matrix($)
{
	my $file = shift;
	my $gotnodenames = 0;
	open INITFILE, $file or die "Fail to open init file $file !\n";
	while (<INITFILE>) {
		next if m/^\s*\#/;
		my @entries = split;
		next if (@entries == 0); 
		
		if ($gotnodenames) {
			return 0 if @entries != $node_count;
			s/INF/1028/i foreach @entries;
			push @cost_matrix, \@entries;
		}
		else {
			$gotnodenames = 1;
			$node_count = @entries;
			@node_ids{@entries} = 0..@entries-1;
		}		
		
	}
	close INITFILE;
	return 1;
}

sub show_cost_metric(\@)
{
	my @matrix = @{$_[0]};
	print "------------------------------------------------------*-\n";
	print "The cost metrics are listed below: \n";
	print "------------------------------------------------------*-\n";
	for my $row(@matrix){
		# print join "\t", @{$cost_matrix[$row]};
		print join "\t", @$row;
		print "\n";
	}
}

sub show_node_entry()
{
	print "------------------------------------------------------*-\n";
	print "Following are the nodes in the graph : total $node_count\n";
	print "------------------------------------------------------*-\n";
	for my $key(keys %node_ids) {
		print "NAME:  $key\tID:  $node_ids{$key}\n";
	}
}

sub extract_rect(\@$$$$)
{
	return 0 if @_ != 5;
	my $AoAref = shift;
	my $xfrom = shift;
	my $xto = shift;
	my $yfrom = shift;
	my $yto = shift;
	my @newAoA;
	
	# Ascertain that $xto >= $xfrom and $yto >= $yfrom
	return 0 if $xto < $xfrom or $yto < $yfrom;
	# Ascertain that $xfrom or $yfrom < 0
	return 0 if $xfrom < 0 or $yfrom < 0;
	# Ascertain that $xto < total available rows
	return 0 if $xto > @$AoAref;
	# Ascertain that $yto < total available columns
	return 0 if $yto > @{$AoAref->[0]}; 

	for my $row($xfrom..$xto) {
		push @newAoA, [@{$AoAref->[$row]}[$yfrom..$yto]];
	}
	return \@newAoA;
}

###
# Purpose:
# 	Use the Floyd all pairs shortest path algorithm to modify the
#	weight matrix to make it contain the minimal cost information
# 	from any pair of nodes. The algorithm runs at O(N**3) 
# Entry:
#	@ - The reference to the cost matrix
# Exit:
#	Returns the modified cost matrix reflecting the minimal cost 
# 	paths for any pair of nodes in the graph. 
# Exceptions:
#

sub do_floyd(\@)
{
	# Reconstrut the matrix
	my @AoA = @{$_[0]};
	my $i = 0;
	my $j = 0;
	my $k = 0;
	my $limit = @AoA - 1;
	for $k(0..$limit) {
		for $i(0..$limit) {
			for $j(0..$limit) {
				$AoA[$i][$j] =
					min ($AoA[$i][$j], 
					$AoA[$i][$k] + $AoA[$k][$j]);			
			}
		}		
	}
}

sub min ($$) 
{
	die "Wrong # of Arguments at min" if @_ != 2;
	my $a = shift;
	my $b = shift;
	return $a < $b ? $a : $b;
}

###
# Purpose:
#	This repetively calls the subroutine explore to use the dijkstra's
#	algorithm to find the minimal weight paths from all nodes to all the
#	the other nodes. 
# Entry:
#	@ - weight matrix containing the weight from any node to any other.
# Exit:
#	Returns the completely modified weight matrix defining the best paths
#	to follow in order to get the minimal cost
# Exceptions:
#

sub do_dijkstra()
{
	# Reconstruct the matrix
	# my @AoA = @{$_[0]};
	for my $src_node (sort keys %node_ids)  {
		explore($src_node);	
	}
}

###
# Purpose:
#	Find the shortest paths starting from the src_node given by $ arg. 
#	The algorithm here used is the Dijkstra's algorithm. It will look
# 	at all the fringe nodes neighboring the current tree. Then the
#	algorithm will pick one candidate node to be included in the current
# 	tree which will produce the smallest distance from the src node.
#	min[ d(src_node, explored_node) + d(explored_node, candidate)]. 
#	This process is done repetitively to explore the whole graph. 
# Entry:
#	$ - $src_node The starting node
#	@ - the matrix defining the distances between the nodes
# Exit:
#	The modified distance (weight) matrix and the routes via which the 
#	src_node follows in order to get the minimal distance. 

sub explore ($)
{
	$_ = shift;
	my $src_node = $node_ids{$_};
	print "------------------------------------------------------*-\n";
	print "From the node $_ : \n";
	print "------------------------------------------------------*-\n";

	my @weights;
	my @explored_nodes; 
	my @metric_vec = @{$cost_matrix[$src_node]};

	# STEP 1: Initialization of the explored nodes
	push @explored_nodes, $src_node;
	# Use the hash of the arrays to store the shortest paths
	my %shortest_paths;
	for my $i(0..$node_count-1) {
		$shortest_paths{$i} = [get_node_name ($src_node)];
	}

	while (@explored_nodes != $node_count) {
		my $min_metric = 1028;
		my $closest_node;

		for my $i(0..$node_count-1) {
			# If the node has been already explored, ignore
			next if has_node_inside ($i, @explored_nodes);
			# Search for the closest neighbor
			if($min_metric > $metric_vec[$i]) {
				$min_metric = $metric_vec[$i];
				$closest_node = $i;
			}

		}
		# STEP 2: Add the closest neighbor to the the explored_nodes list
		push @explored_nodes, $closest_node;
		push @{$shortest_paths{$closest_node}}, get_node_name ($closest_node);
		# STEP 3: Update cost and paths
		for my $i(0..$node_count-1) {
			# If the node has been already explored, ignore
			next if has_node_inside ($i, @explored_nodes);
			my $new_metric = $min_metric + $cost_matrix[$closest_node][$i];
			if ($new_metric < $metric_vec[$i]) {
				$metric_vec[$i] = $new_metric;
				@{$shortest_paths{$i}} = @{$shortest_paths{$closest_node}};
			}

		}
	}
	print join "\t", @metric_vec;
	print "\n";
	for my $i(0..$node_count-1){
		print join "->", @{$shortest_paths{$i}};
		print "\n";
	}

}

###
# Purpose:
#	Returns the name of the node given the numerical id of the node.
#	The program stores the available nodes and their corresponding names
# 	in a hash. Thus this is reverse of indexing of hash, value -> key 
# Entry:
#	$ - the node id, 0 < id < node_count
# Exit:
#	returns the node's name
# Exceptions
#
sub get_node_name ($) 
{
	for my $key (keys %node_ids) {
		return $key if ($node_ids{$key} eq $_[0]);
	}
	return "NOT FOUND!\n";
}

###
# has_node_inside ($\@) test whether the array contains the node.
# Purpose:
# 	Check inside a node list to see whether the list contains the 
#	node specified by the argument
# Entry:
#	$ - the node to be searched 
#	@ - the array of nodes to be searched against
# Exit:
# 	returns either 1 or 0 to indicate whether a node is found or not
# Exceptions:
#

sub has_node_inside ($\@) 
{
	my $tgt = shift;
	$_ = shift;
	my @heap = @$_;

	for my $item(@heap) {
		return 1 if $item == $tgt;
	}
	return 0;
}
