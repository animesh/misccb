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
# Description: Executes a simulation with one LIAF neuron that is stimulated by spikes from a file.
#              Neuron parameters and synapse weight have to be chosen dependent on the test (non-spiking/spiking, marginal threshold intersection)
#              Output files (voltage+spike trace) are stored in directory files/
#
# Usage:       python neuron_compare.py <simulator name> <spikefile> <option> <name suffix> <timestep>
#
#              <simulator name> is the name of the PyNN module, i.e. the module pyNN.<simulator name> is imported
#              <spikefile> is a file that contains the input spike times in ASCII format, one spike time per line
#              <option> is used to choose between different methods for one simulator. These are:
#                     - for NEST:    0 - normal mode (on-grid spiking)
#                                    1 - continuous time mode (off-grid spiking)
#                     - for NEURON:  0 - normal mode (fixed timestep)
#                                    1 - variable timestep mode (CVODE)
#                                    2 - variable timestep mode (CVODE) with non-default tolerances (exponent set by <timestep>)
#                     - for Brian:   0 - normal mode (Euler Forward)
#                                    1 - 2nd order Runge-Kutta integration
#                                    2 - exponential Euler integration
#                                       ( Brian method options need manual extension of pyNN.setup(), see below )
#              <name suffix> suffix to be added to the output file names
#              <timestep> stepsize of the simulation for fixed stepsize methods, in ms. Should be a divider of the connection delay (10 ms);
#                         for NEURON with CVODE and non-default tolerances: set order of magnitude of abstol and reltol
#
#              For usage examples, see the files neuron_sweep.sh and neuronvar_sweep.sh
#------------------------------------------------------------------------

import numpy
import sys

input_file = ''
namesuffix = ''
nameprefix = 'files/'

simulatorname = 'neuron'

cvode_enable = False
use_1e_6 = False
grid_mode = 'on_grid'
brian_method = ''
tstep = 0.1
tolexp = 6 # set non-default order of magnitude for NEURON with CVODE solver

if len(sys.argv) >= 2:
	simulatorname = sys.argv[1]

	if (len(sys.argv) >= 3):
		input_file = sys.argv[2]

	if (len(sys.argv) >= 4):
		cvval = int(sys.argv[3])
		if (simulatorname == 'nest'):
			if (cvval):
				grid_mode = 'off_grid'
		
		elif (simulatorname == 'brian'):
			if (cvval == 1):
				brian_method = 'RK'
			elif (cvval == 2):
				brian_method = 'exponential_Euler'
				
		elif (simulatorname == 'neuron'):
			cvode_enable = bool(cvval)
			if (cvval > 1):
				use_1e_6 = True
			else:
				use_1e_6 = False
		

	if (len(sys.argv) >= 5):
		namesuffix = sys.argv[4]
		
	if (len(sys.argv) >= 6):
		if (use_1e_6):
			tolexp = int(sys.argv[5])
		else:
			tstep = float(sys.argv[5])
		
else:
	print 'Using default values'


print 'Running simulation with simulator='+simulatorname+', input file='+input_file+', cvode='+str(cvode_enable)+', timestep='+str(tstep)


exec("import pyNN.%s as pynn" % simulatorname)


#tdelaydelta = tstep
tdelaydelta = 0.0
conndelay = 10.0 # has to be common multiple of all tested timesteps

# tolerance parameters for NEURON with cvode
curr_rtol = 0.0 #default for NEURON
curr_atol = 1.0e-3 #default for NEURON
tolsuff = ''
if (use_1e_6):
	rtstr = str(tolexp)
	atstr = str(tolexp)
	curr_rtol = float('1.0e-'+rtstr)
	curr_atol = float('1.0e-'+atstr)
	tolsuff = '_r'+rtstr+'a'+atstr
	print curr_rtol, curr_atol
	

pynn.setup(timestep=tstep,min_delay=tstep,max_delay=conndelay,use_cvode=cvode_enable,atol=curr_atol,rtol=curr_rtol,spike_precision=grid_mode, num_method=brian_method)

# neuron params
nparams = {
			'tau_refrac': 2.0,
			'tau_m': 100.0,
			'i_offset': 0.0,
#			'cm': 50.0e-3, # non-spiking tests
#			'v_thresh': 100.0, # non-spiking tests
			'cm': 20.0e-3, # spiking test
			'v_thresh': 1.1, # spiking test
			'tau_syn_E': 1.0,
			'v_rest': 0.0,
			'tau_syn_I': 5.0,
			'v_reset': 0.0
		  }

# values for default spike train
delta_tspike = 3.0
spike_count = 100
simtime = delta_tspike*(spike_count+0.5)

inspikes = [];
if (input_file):
	print 'Loading spikes from file '+input_file
	inspikes = numpy.loadtxt(input_file)
	if (numpy.size(inspikes) < 2):
		inspikes = [inspikes]
	for nsp in range(len(inspikes)):
		inspikes[nsp] *= 1000.0
		
	simtime = inspikes[-1] + 5.0*nparams['tau_m']
else:
	print 'Using default spike train with '+str(spike_count)+' every '+str(delta_tspike)+'ms.'
	for n in range(spike_count):
		inspikes.append((n+1)*delta_tspike-tdelaydelta)


stim = pynn.create(pynn.SpikeSourceArray, cellparams = {'spike_times':inspikes})
neuron = pynn.create(pynn.IF_curr_exp, cellparams = nparams)
pynn.initialize(neuron, 'v', 0.0)
#neuron.set('v_init', 0.0)

pynn.connect(stim,neuron,weight=7.0e-3,delay=conndelay) # for tests with Poisson stimulation
#pynn.connect(stim,neuron,weight=1.155e-2,delay=conndelay) # for testing the threshold crossing


if (simulatorname == 'neuron'):
	if (cvode_enable):
		simulatorname += 'var'
		if (use_1e_6):
			simulatorname += tolsuff
	else:
		simulatorname += 'fix'

if (simulatorname == 'nest'):
	if (grid_mode == 'off_grid'):
		simulatorname += 'var'

if (simulatorname == 'brian'):
	simulatorname += brian_method


pynn.record(neuron,'spikes_pynn.txt')
pynn.record_v(neuron,'vmem_pynn.txt')

pynn.run(simtime)


# special output with times

srecorder = pynn.simulator.recorder_list[0]
vrecorder = pynn.simulator.recorder_list[1]
sdata = srecorder.get()
vdata = vrecorder.get()

# correct time units to seconds
negcount = 0
for entry in vdata:
	entry[1] = 0.001*entry[1]
	if (entry[1] < 0.0):
		entry[1] = 0.0
		negcount += 1

vdata = vdata[negcount:-1,:]

if (len(sdata.shape) == 1):
	sdata = numpy.array(sdata)

if (sdata.size):
	for ndata in range(sdata.shape[0]):
		sdata[ndata,1] = 0.001*sdata[ndata,1]

if (not cvode_enable):
	namesuffix += 'tstep%04dus' % int(1000.0*tstep)

if (vdata.size):
	numpy.savetxt(nameprefix+'vmem_'+simulatorname+namesuffix+'.txt', vdata, fmt='%d\t%.18e\t%.18e')

if (sdata.size):
	numpy.savetxt(nameprefix+'spikes_'+simulatorname+namesuffix+'.txt', sdata, fmt='%d\t%.18e')


pynn.end(compatible_output=False)
