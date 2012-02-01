import nest
import nest.topology as topo
import math
import pylab
nest.ResetKernel()

excitatory_dict = {
"rows": 30,
"columns": 30,
"extent": [2.0, 2.0],
"elements": "iaf_neuron",
"edge_wrap": True}

inhibitory_dict = {
"rows": 15,
"columns": 15,
"extent": [2.0, 2.0],
"center": [0.0, 0.0],
"elements": "iaf_neuron",
"edge_wrap": True}

exc = topo.CreateLayer(excitatory_dict)
inh = topo.CreateLayer(inhibitory_dict)

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
"weights": 4.0, # the weight of inhibitory connections are four times as high as excitatory.
"delays": 1.5,
"kernel": {"gaussian": {"sigma": 0.3, "p_center": 1.3}},
"allow_autapses": True,
"allow_multapses": True,
"number_of_connections": 22}

topo.ConnectLayers(exc,exc,exc_par)
topo.ConnectLayers(exc,inh,exc_par)
topo.ConnectLayers(inh,inh,inh_par)
topo.ConnectLayers(inh,exc,inh_par)


import numpy as np
pos=[[np.random.uniform(-0.5,0.5),np.random.uniform(-0.5,0.5)]
     for j in xrange(50)]
excr=topo.CreateLayer({'positions':pos,'elements':'iaf_neuron'})
nest.PrintNetwork(depth=2)

#excr_par = {"connection_type": "convergent",
#"mask": {"doughnut": {"inner_radius": 0.1, "outer_radius": 0.3}},
#"mask": {"circular": {"radius": 0.5}},
#"weights": 1.0,
#"delays": 1.5,
#"kernel": {"gaussian": {"sigma": 0.3,
#"p_center": 1.0}},
#"number_of_connections": 10}
topo.ConnectLayers(exc,excr,exc_par)
#topo.ConnectLayers(excr,inh,exc_par)



pois = nest.Create("poisson_generator")
nest.DivergentConnect(pois, nest.GetNodes(exc)[0])
nest.DivergentConnect(pois, nest.GetNodes(inh)[0])




#topo.PrintLayerConnection(exc, 'static_synapse', 'out.txt')


# Create two neuron types
#nest.CopyModel("iaf_neuron", "my_excitatory")
#nest.CopyModel("iaf_neuron", "my_inhibitory")
# Create a single layer
#dict = {"rows": 3,"columns": 4,"extent": [1.0, 1.0],"elements": ["my_excitatory", ["my_inhibitory", "my_excitatory"]]}
#layer = topo.CreateLayer(dict)
# Connect layer to itself
#dict = {"connection_type": "divergent","mask": {"circular": {"radius": 0.1}},"targets": {"lid : 2,"model": "my_excitatory"}}
#dict = {"connection_type": "divergent","mask": {"doughnut": {"inner_radius": 0.1,"outer_radius": 0.3}},"number_of_connections": 100,"allow_multapses": False}
#topo.ConnectLayers(layer, layer, dict)
#layer = topo.CreateLayer({"rows": 5,"columns": 4,"extent": [1.0, 1.0],"elements": "iaf_neuron"})
#node_a = [nest.GetLeaves(layer)[0][2]]
#node_b = [nest.GetLeaves(layer)[0][3]]
#print topo.GetRelativeDistance(node_a, node_b)
#topo.LayerGidPositionMap(layer, 'out.txt')
#topo.PrintLayerConnections(layer, 'static_synapse', 'out.txt')
