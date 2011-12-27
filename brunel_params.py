# -----------------------------------------------------------------------
# SUPPLEMENTARY MATERIAL to the article:
#
# Stephan Henker, Johannes Partzsch and Rene Schueffny (2011):
# Accuracy evaluation of numerical methods used in state-of-the-art simulators for spiking neural networks,
# Journal of Computational Neuroscience
#
# -----------------------------------------------------------------------
# Company         :   Technische Universitaet Dresden                      
# Author          :   partzsch            
# E-Mail          :   Johannes.Partzsch@tu-dresden.de                	
#------------------------------------------------------------------------
# Description: Generate parameters and netlist for the network experiment after (Brunel, 2000)
#              Writes parameters to pickle file brunel_params.py
#
# Usage:       python brunel_params.py
#
#------------------------------------------------------------------------



# helper function: reads netlist from pyNN-compatible connection file (code from pyNN connectors.py)

def getNetlistFromFile(filename):
	f = open(filename, 'r', 10000)
	lines = f.readlines()
	f.close()
	# gather all the data in a list of tuples (one per line)
	curr_list = []
	for line in lines:
		single_line = line.rstrip()
		if (single_line[0] != '#'):
			#print single_line
			src, tgt, dummy1, dummy2 = single_line.split("\t", 4)
			#print src
			#src = "[%s" % src.split("[",1)[1]
			#tgt = "[%s" % tgt.split("[",1)[1]
			curr_list.append( [int(eval(src)), int(eval(tgt))] )

	return curr_list

def writeCompatibleNetlist(netlist,filename):
	f = open(filename, 'w')
	for entry in netlist:
		f.write(str(entry[0])+'\t'+str(entry[1])+'\n')
	
	f.close()


# generate parameters and netlist for Brunel experiment

import pickle as mypic


generate_netlist = False # set to True if you want to generate a new netlist
write_netlist = generate_netlist

# neuron params
nparams = {
			'tau_refrac': 2.0,
			'tau_m': 20.0,
			'i_offset': 0.0,
			'cm': 0.02,
			'v_thresh': 20.0,
			'tau_syn_E': 0.2,
			'v_rest': 0.0,
			'tau_syn_I': 0.2,
			'v_reset': 10.0
		  }


N = 150
Ce = 50.0
J = 0.2 # in mV


Nexc = int(0.8*N)
Ninh = int(0.2*N)
Ci = 0.25*Ce
Next = Nexc
Cext = Ce

p_ext = Cext/Next
p_exc = 6.0/120.0 # reduced for less chaotic behaviour
p_inh = p_exc


if (generate_netlist):
	import pyNN.nest as pynn

	pynn.setup()

	pop_ext = pynn.Population(Next, pynn.SpikeSourcePoisson)
	pop_exc = pynn.Population(Nexc, pynn.IF_curr_exp, cellparams = nparams)
	pop_inh = pynn.Population(Ninh, pynn.IF_curr_exp, cellparams = nparams)

	proj_extexc = pynn.Projection(pop_ext,pop_exc, pynn.FixedProbabilityConnector(p_connect=p_ext), target='excitatory' )
	proj_extinh = pynn.Projection(pop_ext,pop_inh, pynn.FixedProbabilityConnector(p_connect=p_ext), target='excitatory' )

	proj_excexc = pynn.Projection(pop_exc,pop_exc, pynn.FixedProbabilityConnector(p_connect=p_exc), target='excitatory' )
	proj_excinh = pynn.Projection(pop_exc,pop_inh, pynn.FixedProbabilityConnector(p_connect=p_exc), target='excitatory' )
	proj_inhexc = pynn.Projection(pop_inh,pop_exc, pynn.FixedProbabilityConnector(p_connect=p_inh), target='inhibitory' )
	proj_inhinh = pynn.Projection(pop_inh,pop_inh, pynn.FixedProbabilityConnector(p_connect=p_inh), target='inhibitory' )

	proj_extexc.saveConnections('conn_extexc.tmp',compatible_output=True)
	proj_extinh.saveConnections('conn_extinh.tmp',compatible_output=True)
	proj_excexc.saveConnections('conn_excexc.tmp',compatible_output=True)
	proj_excinh.saveConnections('conn_excinh.tmp',compatible_output=True)
	proj_inhexc.saveConnections('conn_inhexc.tmp',compatible_output=True)
	proj_inhinh.saveConnections('conn_inhinh.tmp',compatible_output=True)

	pynn.end()



# read back entries to include them in params
net_extexc = getNetlistFromFile('conn_extexc.tmp')
net_extinh = getNetlistFromFile('conn_extinh.tmp')
net_excexc = getNetlistFromFile('conn_excexc.tmp')
net_excinh = getNetlistFromFile('conn_excinh.tmp')
net_inhexc = getNetlistFromFile('conn_inhexc.tmp')
net_inhinh = getNetlistFromFile('conn_inhinh.tmp')


brunel_params = {	'N' : N,
					'Ce' : Ce,
					'J' : J,
					'Nexc' : Nexc,
					'Ninh' : Ninh,
					'Ci' : Ci,
					'Next' : Next,
					'Cext' : Cext,
					'p_ext' : p_ext,
					'p_exc' : p_exc,
					'p_inh' : p_inh,
					'nparams' : nparams,
					
					'net_extexc' : net_extexc,
					'net_extinh' : net_extinh,
					'net_excexc' : net_excexc,
					'net_excinh' : net_excinh,
					'net_inhexc' : net_inhexc,
					'net_inhinh' : net_inhinh
				}

# write parameters to file

fout = open('brunel_params.pkl', 'wb')

mypic.dump(brunel_params, fout)

fout.close()


if (write_netlist):
	writeCompatibleNetlist(net_extexc,'netlist_extexc.txt')
	writeCompatibleNetlist(net_extinh,'netlist_extinh.txt')
	writeCompatibleNetlist(net_excexc,'netlist_excexc.txt')
	writeCompatibleNetlist(net_excinh,'netlist_excinh.txt')
	writeCompatibleNetlist(net_inhexc,'netlist_inhexc.txt')
	writeCompatibleNetlist(net_inhinh,'netlist_inhinh.txt')



