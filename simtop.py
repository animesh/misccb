import nest
import nest.topology as topo
import numpy as np
import math
import pylab
import time
foo='simtop.'+str(time.time())
nest.ResetKernel()

sim_int=5.0
sim_time=500.0

excitatory_dict = {
    "rows": 30,
    "columns": 30,
    "extent": [2.0, 2.0],
    "center": [0.0, 0.0],
    "elements": "iaf_cond_alpha",
    "edge_wrap": True
    }

inhibitory_dict = {
    "rows": 15,
    "columns": 15,
    "extent": [2.0, 2.0],
    "center": [0.0, 0.0],
    "elements": "iaf_neuron",
    "edge_wrap": True
    }

mix_dict = {
    "rows": 10,
    "columns": 10,
    "extent": [2.0, 2.0],
    "center": [0.0, 0.0],
    "elements": "iaf_neuron",
    "edge_wrap": True
    }

exc = topo.CreateLayer(excitatory_dict)
inh = topo.CreateLayer(inhibitory_dict)
mix = topo.CreateLayer(mix_dict)

exc_par = {
    "connection_type": "convergent",
    "mask": {"circular": {"radius": 1.0}},
    "weights": 40.0,
    "delays": 1.5,
    "kernel": {"gaussian": {"sigma": 0.3,"p_center": 1.3}},
    "allow_autapses": True,
    "allow_multapses": True,
    "number_of_connections": 90
    }

inh_par = {
    "connection_type": "convergent",
    "mask": {"circular": {"radius": 0.5}},
    "weights": -10.0,
    "delays": 1.5,
    "kernel": {"gaussian": {"sigma": 0.3, "p_center": 1.3}},
    "allow_autapses": True,
    "allow_multapses": True,
    "number_of_connections": 22
    }

mix_par = {
    "connection_type": "convergent",
    "mask": {"circular": {"radius": 0.5}},
    "weights": -20.0, 
    "delays": 1.0,
    "kernel": {"gaussian": {"sigma": 0.3, "p_center": 1.3}},
    "allow_autapses": True,
    "allow_multapses": True,
    "number_of_connections": 12
    }

topo.ConnectLayers(exc,exc,exc_par)
topo.ConnectLayers(exc,inh,exc_par)
topo.ConnectLayers(inh,inh,inh_par)
topo.ConnectLayers(inh,exc,inh_par)
topo.ConnectLayers(exc,mix,exc_par)
topo.ConnectLayers(mix,inh,exc_par)
topo.ConnectLayers(mix,exc,inh_par)

nest.PrintNetwork(depth=2)

pois = nest.Create("poisson_generator")
gex = nest.Create('spike_generator',
                  params = {'spike_times': np.array([10.0, 20.0, 50.0])})
gin = nest.Create('spike_generator',
                  params = {'spike_times': np.array([15.0, 25.0, 55.0])})

nest.DivergentConnect(gex, nest.GetNodes(exc)[0])
nest.DivergentConnect(gin, nest.GetNodes(inh)[0])
nest.DivergentConnect(pois, nest.GetNodes(mix)[0])

mixpos = zip(*[topo.GetPosition([n]) for n in nest.GetLeaves(mix)[0]])[0]
#pylab.plot(mixpos[0], mixpos[1], 'x')
#pylab.show()

#nest.CopyModel('multimeter', 'RecordingNode',
#               params = {'withtime': True, 
#                         'withgid': True,
#                         'to_file': True,
#                         'label': foo,
#                         'interval': sim_int,
#                         'record_from': ['V_m', 'g_ex', 'g_in'],
#                         'withpath'   : True })

mixmm=nest.Create('multimeter',
               params = {'withtime': True, 
                         'withgid': True,
                         'to_file': True,
                         'label': foo+'mixmm',
                         'interval': sim_int,
                         'record_from': ['V_m', 'g_ex', 'g_in'],
                         'withpath'   : True })

#mm=nest.Create('RecordingNode')
#nest.DivergentConnect(mm, nest.GetNodes(exc)[0])
tgts = [nd for nd in nest.GetLeaves(mix)[0] 
        if nest.GetStatus([nd], 'model')[0]==model]
nest.DivergentConnect(mixmm,tgts)

#recorders = {}
#for name, loc, population, model in [
    #('mix',1,mix,'mix'),
    #('inh',2,inh , 'inh'),
    #('exc', 3, exc, 'exc')
    #]:
    #nest.DivergentConnect(mm,name)
    #recorders[name] = (nest.Create('RecordingNode'), loc)
    #tgts = [nd for nd in nest.GetLeaves(population)[0] 
    #        if nest.GetStatus([nd], 'model')[0]==model]
    #nest.DivergentConnect(recorders[name][0], tgts) 

nest.SetStatus([0],{'print_time': True})

nest.Simulate(sim_int)

#for t in  pylab.arange(sim_int, sim_time, sim_int):
for t in  range(0, 0):
    print t
    nest.Simulate(sim_int)
    #pylab.clf()
    #pylab.jet()
    for name, r in recorders.iteritems():
        rec = r[0]
        sp = r[1]
        #pylab.subplot(2,2,sp)
        d = nest.GetStatus(rec)[0]['events']['V_m']
        nest.SetStatus(rec, {'n_events': 0})
        print rec,sp,d,nest.GetKernelStatus()['time']
        #pylab.title(name + ', t = %6.1f ms' % nest.GetKernelStatus()['time'])
#    pylab.show()





print nest.GetKernelStatus()
topo.DumpLayerNodes(mix,  'mix.dat')
topo.DumpLayerConnections(mix, 'static_synapse', 'mix.c.dat')



