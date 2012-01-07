"""
Small network created with the Population and Projection classes

Andrew Davison, UNIC, CNRS
May 2006

$Id: small_network.py 771 2010-07-30 02:08:40Z apdavison $

"""

import numpy
from pyNN.utility import get_script_args

simulator_name = get_script_args(1)[0]  
exec("from pyNN.%s import *" % simulator_name)

# === Define parameters ========================================================

n = 5    # Number of cells
w = 0.2   # synaptic weight (nA)
cell_params = {
    'tau_m'      : 20.0, # (ms)
    'tau_syn_E'  : 2.0,  # (ms)
    'tau_syn_I'  : 4.0,  # (ms)
    'tau_refrac' : 2.0,  # (ms)
    'v_rest'     : 0.0,  # (mV)
    'v_reset'    : 0.0,  # (mV)
    'v_thresh'   : 20.0, # (mV)
    'cm'         : 0.5}  # (nF)
dt         = 0.1         # (ms)
syn_delay  = 1.0         # (ms)
input_rate = 50.0       # (Hz)
simtime    = 1000.0      # (ms)

# === Build the network ========================================================

setup(timestep=dt, max_delay=syn_delay)

cells = Population(n, IF_curr_alpha, cell_params, label="cells")

number = int(2*simtime*input_rate/1000.0)
numpy.random.seed(26278342)
spike_times = numpy.add.accumulate(numpy.random.exponential(1000.0/input_rate, size=number))
assert spike_times.max() > simtime

spike_source = Population(n, SpikeSourceArray, {'spike_times': spike_times})

cells.record()
cells.record_v()

input_conns = Projection(spike_source, cells, AllToAllConnector())
input_conns.setWeights(w)
input_conns.setDelays(syn_delay)

# === Run simulation ===========================================================

cells.initialize('v', 0.0)  # (mV)
run(simtime)

cells.printSpikes("Results/small_network_%s.ras" % simulator_name)
cells.print_v("Results/small_network_%s.v" % simulator_name)

print "Mean firing rate: ", cells.meanSpikeCount()*1000.0/simtime, "Hz"

# === Clean up and quit ========================================================

end()
