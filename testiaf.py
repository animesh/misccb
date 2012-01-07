#! /usr/bin/env python

#
# A DC current is injected into the neuron using a current generator 
# device. The membrane potential as well as the spiking activity are 
# recorded by corresponding devices.
#
# It can be observed how the current charges the membrane, a spike
# is emitted, the neuron becomes absolute refractory, and finally
# starts to recover.
#
# Author: Jochen Martin Eppler, 07/2007
# Based on test_iaf.sli by Markus Diesmann
#

import nest
import pylab

def build_network(dt) :

    nest.ResetKernel()
    nest.SetKernelStatus({"local_num_threads" : 1, "resolution" : dt})

    neuron = nest.Create('iaf_neuron')
    nest.SetStatus(neuron, "I_e", 376.0)

    vm = nest.Create('voltmeter')
    nest.SetStatus(vm, "withtime", True)
    
    sd = nest.Create('spike_detector')

    nest.Connect(vm, neuron)
    nest.Connect(neuron, sd)

    return vm, sd
    
if __name__ == "__main__" :

    for dt in [0.1, 0.5, 1.0] :
        print "Running simulation with dt=%.2f" % dt
        vm, sd = build_network(dt)
        nest.Simulate(1000.0)
        potentials = nest.GetStatus(vm, "events")[0]["V_m"]
        times = nest.GetStatus(vm, "events")[0]["times"]
        pylab.plot(times, potentials, label="dt=%.2f" % dt)
        print "  Number of spikes:",  nest.GetStatus(sd, "n_events")[0]

    pylab.legend(loc=3)
    pylab.xlabel("time (ms)")
    pylab.ylabel("V_m (mV)")
    pylab.show()
