import nest
import nest.topology as topo
import math
import pylab
nest.ResetKernel()

sim_int=5.0
sim_time=100.0

excitatory_dict = {
"rows": 30,
"columns": 30,
"extent": [2.0, 2.0],
"center": [0.0, 0.0],
"elements": "iaf_neuron",
"edge_wrap": True}

inhibitory_dict = {
"rows": 15,
"columns": 15,
"extent": [2.0, 2.0],
"center": [0.0, 0.0],
"elements": "iaf_neuron",
"edge_wrap": True}

mix_dict = {
"rows": 10,
"columns": 10,
"extent": [2.0, 2.0],
"center": [0.0, 0.0],
"elements": "iaf_neuron",
"edge_wrap": True}

exc = topo.CreateLayer(excitatory_dict)
inh = topo.CreateLayer(inhibitory_dict)
mix = topo.CreateLayer(mix_dict)

exc_par = {"connection_type": "convergent",
"mask": {"circular": {"radius": 1.0}},
"weights": 1.0,
"delays": 1.5,
"kernel": {"gaussian": {"sigma": 0.3,
"p_center": 1.3}},
"allow_autapses": True,
"allow_multapses": True,
"number_of_connections": 90}

inh_par = {"connection_type": "convergent",
"mask": {"circular": {"radius": 0.5}},
"weights": -4.0, # the weight of inhibitory connections are four times as high as excitatory.
"delays": 1.5,
"kernel": {"gaussian": {"sigma": 0.3, "p_center": 1.3}},
"allow_autapses": True,
"allow_multapses": True,
"number_of_connections": 22}

mix_par = {"connection_type": "convergent",
"mask": {"circular": {"radius": 0.5}},
"weights": -2.0, # the weight of inhibitory connections are four times as high as excitatory.
"delays": 1.0,
"kernel": {"gaussian": {"sigma": 0.3, "p_center": 1.3}},
"allow_autapses": True,
"allow_multapses": True,
"number_of_connections": 22}

topo.ConnectLayers(exc,exc,exc_par)
topo.ConnectLayers(exc,inh,exc_par)
topo.ConnectLayers(inh,inh,inh_par)
topo.ConnectLayers(inh,exc,inh_par)
topo.ConnectLayers(exc,mix,mix_par)
topo.ConnectLayers(mix,inh,mix_par)

nest.PrintNetwork(depth=2)

mixpos = zip(*[topo.GetPosition([n]) for n in nest.GetLeaves(mix)[0]])[0]
#pylab.plot(mixpos[0], mixpos[1], 'x')
#pylab.show()

nest.CopyModel('multimeter', 'RecordingNode',
               params = {'interval'   : sim_int,
                         'record_from': ['V_m'],
                         'record_to'  : ['memory'],
                         'withgid'    : True,
                         'withpath'   : True,
                         'withtime'   : True})

recorders = {}
for name, loc, population, model in [('mix'   , 1, mix  , 'mix'),
                                     ('inh'        , 2, inh  , 'inh'),
                                     ('exc', 3, exc, 'exc')]:
    recorders[name] = (nest.Create('RecordingNode'), loc)
    tgts = [nd for nd in nest.GetLeaves(population)[0] 
            if nest.GetStatus([nd], 'model')[0]==model]
    nest.DivergentConnect(recorders[name][0], tgts) 

nest.SetStatus([0],{'print_time': True})

nest.Simulate(sim_int)

N=30
for t in pylab.arange(sim_int, sim_time, sim_int):
    print t
    nest.Simulate(sim_int)
    pylab.clf()
    pylab.jet()
    for name, r in recorders.iteritems():
        rec = r[0]
        sp = r[1]
        pylab.subplot(2,2,sp)
        d = nest.GetStatus(rec)[0]['events']['V_m']
        nest.SetStatus(rec, {'n_events': 0})
        print rec,sp,d,nest.GetKernelStatus()['time']
        #pylab.title(name + ', t = %6.1f ms' % nest.GetKernelStatus()['time'])
#    pylab.show()




pois = nest.Create("poisson_generator")
nest.DivergentConnect(pois, nest.GetNodes(exc)[0])
nest.DivergentConnect(pois, nest.GetNodes(inh)[0])

print nest.GetKernelStatus()
topo.DumpLayerNodes(mix,  'mix.txt')
topo.DumpLayerConnections(mix, 'static_synapse', 'mixc.txt')



