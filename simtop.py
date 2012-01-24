#! :Author: Hans Ekkehard Plesser
#! :Copyright: The NEST Initiative (2009)
#! :License: Creative Commons Attribution License
#!   S. L. Hill and G. Tononi.
#!   Modeling Sleep and Wakefulness in the Thalamocortical System.
import nest
import nest.topology as topo
nest.ResetKernel()
import math
import pylab
Params = {'N'           :     40,
          'visSize'     :    8.0,
          'f_dg'        :    2.0,
          'lambda_dg'   :    2.0,
          'phi_dg'      :    0.0,
          'retDC'       :   30.0,
          'retAC'       :   30.0,
          'simtime'     : 100.0,
          'sim_interval':   5.0
          }

nest.CopyModel('iaf_cond_alpha', 'NeuronModel',
               params = {'C_m'       :  16.0,
                         'E_L'       : (0.2 * 30.0 + 1.5 * -90.0)/(0.2 + 1.5),
                         'g_L'       : 0.2 + 1.5, 
                         'E_ex'      :   0.0,
                         'E_in'      : -70.0,
                         'V_reset'   : -60.0,
                         'V_th'      : -51.0,
                         't_ref'     :   2.0,
                         'tau_syn_ex':   1.0,
                         'tau_syn_in':   2.0,
                         'I_e'       :   0.0,
                         'V_m'       : -70.0})

nest.CopyModel('NeuronModel', 'CtxExNeuron')

nest.CopyModel('NeuronModel', 'CtxInNeuron', 
               params = {'C_m'  :   8.0,
                         'V_th' : -53.0,
                         't_ref':   1.0})

nest.CopyModel('NeuronModel', 'ThalamicNeuron', 
               params = {'C_m'  :   8.0,
                         'V_th' : -53.0,
                         't_ref':   1.0,
                         'E_in' : -80.0})

def phiInit(pos, lam, alpha):
    '''Initializer function for phase of drifting grating nodes.

       pos  : position (x,y) of node, in degree
       lam  : wavelength of grating, in degree
       alpha: angle of grating in radian, zero is horizontal

       Returns number to be used as phase of AC Poisson generator.
    '''
    return 2.0 * math.pi / lam * (math.cos(alpha) * pos[0] + math.sin(alpha) * pos[1]) 

nest.CopyModel('smp_generator', 'RetinaNode',
               params = {'ac'    : Params['retAC'],
                         'dc'    : Params['retDC'],
                         'freq'  : Params['f_dg'],
                         'phi'   : 0.0})

nest.CopyModel('multimeter', 'RecordingNode',
               params = {'interval'   : Params['sim_interval'],
                         'record_from': ['V_m'],
                         'record_to'  : ['memory'],
                         'withgid'    : True,
                         'withpath'   : False,
                         'withtime'   : False})


layerProps = {'rows'     : Params['N'], 
              'columns'  : Params['N'],
              'extent'   : [Params['visSize'], Params['visSize']],
              'edge_wrap': True}
#! This dictionary does not yet specify the elements to put into the
#! layer, since they will differ from layer to layer. We will add them
#! below by updating the ``'elements'`` dictionary entry for each
#! population.

#! Retina
#! ------
layerProps.update({'elements': 'RetinaNode'})
retina = topo.CreateLayer(layerProps)

#! Now set phases of retinal oscillators; we use a list comprehension instead
#! of a loop.
[nest.SetStatus([n], {"phi": phiInit(topo.GetPosition([n])[0], 
                                      Params["lambda_dg"],
                                      Params["phi_dg"])})
 for n in nest.GetLeaves(retina)[0]]

#! Thalamus
#! --------

#! We first introduce specific neuron models for the thalamic relay
#! cells and interneurons. These have identical properties, but by
#! treating them as different models, we can address them specifically
#! when building connections.
#!
#! We use a list comprehension to do the model copies.
[nest.CopyModel('ThalamicNeuron', SpecificModel) for SpecificModel in ('TpRelay', 'TpInter')]

#! Now we can create the layer, with one relay cell and one
#! interneuron per location:
layerProps.update({'elements': ['TpRelay', 'TpInter']})
Tp = topo.CreateLayer(layerProps)

#! Reticular nucleus
#! -----------------
#! We follow the same approach as above, even though we have only a
#! single neuron in each location.
[nest.CopyModel('ThalamicNeuron', SpecificModel) for SpecificModel in ('RpNeuron',)]
layerProps.update({'elements': 'RpNeuron'})
Rp = topo.CreateLayer(layerProps)

#! Primary visual cortex
#! ---------------------

#! We follow again the same approach. We differentiate neuron types
#! between layers and between pyramidal cells and interneurons. At
#! each location, there are two pyramidal cells and one interneuron in
#! each of layers 2-3, 4, and 5-6. Finally, we need to differentiate
#! between vertically and horizontally tuned populations. When creating
#! the populations, we create the vertically and the horizontally
#! tuned populations as separate populations.

#! We use list comprehesions to create all neuron types:
[nest.CopyModel('CtxExNeuron', layer+'pyr') for layer in ('L23','L4','L56')]
[nest.CopyModel('CtxInNeuron', layer+'in' ) for layer in ('L23','L4','L56')]

#! Now we can create the populations, suffixes h and v indicate tuning
layerProps.update({'elements': [['L23pyr', 2, 'L23in', 1],
                                ['L4pyr' , 2, 'L4in' , 1],
                                ['L56pyr', 2, 'L56in', 1]]})
Vp_h = topo.CreateLayer(layerProps)
Vp_v = topo.CreateLayer(layerProps)

#! Collect all populations
#! -----------------------

#! For reference purposes, e.g., printing, we collect all populations
#! in a tuple:
populations = (retina, Tp, Rp, Vp_h, Vp_v)

#! Inspection
#! ----------

#! We can now look at the network using `PrintNetwork`:
nest.PrintNetwork()

#! We can also try to plot a single layer in a network. For
#! simplicity, we use Rp, which has only a single neuron per position.
Rppos = zip(*[topo.GetPosition([n]) for n in nest.GetLeaves(Rp)[0]])[0]
#! The line above works as follows:
#!
#! 1. ``GetLeaves`` extracts all neurons from the `Rp` layer
#! #. For each neuron ``n``, ``GetPosition`` obtains the position 
#!    within the layer, in degrees.
#! #. The list comprehension ``[... for n in ...]`` results in a list
#!    of x-y-coordinate pairs. ``zip(*[...])`` turns this into one list
#!    of x- and one list of y-coordinates, as needed by ``plot``.
pylab.plot(Rppos[0], Rppos[1], 'o')
axsz = Params['visSize']/2 + 0.2
pylab.axis([-axsz,axsz,-axsz,axsz])
pylab.title('Layer Rp')
pylab.show()


#! Synapse models
#! ==============

#! Actual synapse dynamics, e.g., properties such as the synaptic time
#! course, time constants, reversal potentials, are properties of
#! neuron models in NEST and we set them in section `Neuron models`_
#! above. When we refer to *synapse models* in NEST, we actually mean
#! connectors which store information about connection weights and
#! delays, as well as port numbers at the target neuron (``rport``)
#! and implement synaptic plasticity. The latter two aspects are not
#! relevant here.
#!
#! We just use NEST's ``static_synapse`` connector but copy it to
#! synapse models ``AMPA`` and ``GABA_A`` for the sake of
#! explicitness. Weights and delays are set as needed in section
#! `Connections`_ below, as they are different from projection to
#! projection. De facto, the sign of the synaptic weight decides
#! whether input via a connection is handle by the ``_ex`` or the
#! ``_in`` synapse.
nest.CopyModel('static_synapse', 'AMPA')
nest.CopyModel('static_synapse', 'GABA_A')

#! Connections
#! ====================

#! Building connections is the most complex part of network
#! construction. Connections are specified in Table 1 in the
#! Hill-Tononi paper. As pointed out above, we only consider AMPA and
#! GABA_A synapses here.  Adding other synapses is tedious work, but
#! should pose no new principal challenges. We also use a uniform in
#! stead of a Gaussian distribution for the weights.
#!
#! The model has two identical primary visual cortex populations,
#! ``Vp_v`` and ``Vp_h``, tuned to vertical and horizonal gratings,
#! respectively. The *only* difference in the connection patterns
#! between the two populations is the thalamocortical input to layers
#! L4 and L5-6 is from a population of 8x2 and 2x8 grid locations,
#! respectively. Furthermore, inhibitory connection in cortex go to 
#! the opposing orientation population as to the own.
#!
#! To save us a lot of code doubling, we thus defined properties
#! dictionaries for all connections first and then use this to connect
#! both populations. We follow the subdivision of connections as in
#! the Hill & Tononi paper.
#!
#! **Note:** Hill & Tononi state that their model spans 8 degrees of
#! visual angle and stimuli are specified according to this. On the
#! other hand, all connection patterns are defined in terms of cell
#! grid positions. Since the NEST Topology Module defines connection
#! patterns in terms of the extent given in degrees, we need to apply
#! the following scaling factor to all lengths in connections:
dpc = Params['visSize'] / (Params['N'] - 1)

#! We will collect all same-orientation cortico-cortical connections in
ccConnections = []
#! the cross-orientation cortico-cortical connections in
ccxConnections = []
#! and all cortico-thalamic connections in 
ctConnections = []

#! Horizontal intralaminar
#! -----------------------
#! *Note:* "Horizontal" means "within the same cortical layer" in this
#! case.
#!
#! We first define a dictionary with the (most) common properties for
#! horizontal intralaminar connection. We then create copies in which
#! we adapt those values that need adapting, and  
horIntraBase = {"connection_type": "divergent",
                "synapse_model": "AMPA",
                "mask": {"circular": {"radius": 12.0 * dpc}},
                "kernel": {"gaussian": {"p_center": 0.05, "sigma": 7.5 * dpc}},
                "weights": 1.0,
                "delays": {"uniform": {"min": 1.75, "max": 2.25}}}

#! We use a loop to do the for for us. The loop runs over a list of 
#! dictionaries with all values that need updating
for conn in [{"sources": {"model": "L23pyr"}, "targets": {"model": "L23pyr"}},
             {"sources": {"model": "L23pyr"}, "targets": {"model": "L23in" }},
             {"sources": {"model": "L4pyr" }, "targets": {"model": "L4pyr" },
              "mask"   : {"circular": {"radius": 7.0 * dpc}}},
             {"sources": {"model": "L4pyr" }, "targets": {"model": "L4in"  },
              "mask"   : {"circular": {"radius": 7.0 * dpc}}},
             {"sources": {"model": "L56pyr"}, "targets": {"model": "L56pyr" }},
             {"sources": {"model": "L56pyr"}, "targets": {"model": "L56in"  }}]:
    ndict = horIntraBase.copy()
    ndict.update(conn)
    ccConnections.append(ndict)

#! Vertical intralaminar
#! -----------------------
#! *Note:* "Vertical" means "between cortical layers" in this
#! case.
#!
#! We proceed as above.
verIntraBase = {"connection_type": "divergent",
                "synapse_model": "AMPA",
                "mask": {"circular": {"radius": 2.0 * dpc}},
                "kernel": {"gaussian": {"p_center": 1.0, "sigma": 7.5 * dpc}},
                "weights": 2.0,
                "delays": {"uniform": {"min": 1.75, "max": 2.25}}}

for conn in [{"sources": {"model": "L23pyr"}, "targets": {"model": "L56pyr"}, "weights": 1.0},
             {"sources": {"model": "L23pyr"}, "targets": {"model": "L23in" }, "weights": 1.0},
             {"sources": {"model": "L4pyr" }, "targets": {"model": "L23pyr"}},
             {"sources": {"model": "L4pyr" }, "targets": {"model": "L23in" }},
             {"sources": {"model": "L56pyr"}, "targets": {"model": "L23pyr"}},
             {"sources": {"model": "L56pyr"}, "targets": {"model": "L23in" }},
             {"sources": {"model": "L56pyr"}, "targets": {"model": "L4pyr" }},
             {"sources": {"model": "L56pyr"}, "targets": {"model": "L4in"  }}]:
    ndict = verIntraBase.copy()
    ndict.update(conn)
    ccConnections.append(ndict)

#! Intracortical inhibitory
#! ------------------------
#!
#! We proceed as above, with the following difference: each connection 
#! is added to the same-orientation and the cross-orientation list of
#! connections.
#!
#! **Note:** Weights increased from -1.0 to -2.0, to make up for missing GabaB
#!
#! Note that we have to specify the **weight with negative sign** to make
#! the connections inhibitory.
intraInhBase = {"connection_type": "divergent",
                "synapse_model": "GABA_A",
                "mask": {"circular": {"radius": 7.0 * dpc}},
                "kernel": {"gaussian": {"p_center": 0.25, "sigma": 7.5 * dpc}},
                "weights": -2.0,
                "delays": {"uniform": {"min": 1.75, "max": 2.25}}}

#! We use a loop to do the for for us. The loop runs over a list of 
#! dictionaries with all values that need updating
for conn in [{"sources": {"model": "L23in"}, "targets": {"model": "L23pyr"}},
             {"sources": {"model": "L23in"}, "targets": {"model": "L23in" }},
             {"sources": {"model": "L4in" }, "targets": {"model": "L4pyr" }},
             {"sources": {"model": "L4in" }, "targets": {"model": "L4in"  }},
             {"sources": {"model": "L56in"}, "targets": {"model": "L56pyr"}},
             {"sources": {"model": "L56in"}, "targets": {"model": "L56in" }}]:
    ndict = intraInhBase.copy()
    ndict.update(conn)
    ccConnections.append(ndict)
    ccxConnections.append(ndict)

#! Corticothalamic
#! ---------------
corThalBase = {"connection_type": "divergent",
               "synapse_model": "AMPA",
               "mask": {"circular": {"radius": 5.0 * dpc}},
               "kernel": {"gaussian": {"p_center": 0.5, "sigma": 7.5 * dpc}},
               "weights": 1.0,
               "delays": {"uniform": {"min": 7.5, "max": 8.5}}}

#! We use a loop to do the for for us. The loop runs over a list of 
#! dictionaries with all values that need updating
for conn in [{"sources": {"model": "L56pyr"}, "targets": {"model": "TpRelay" }},
             {"sources": {"model": "L56pyr"}, "targets": {"model": "TpInter" }}]:
    ndict = intraInhBase.copy()
    ndict.update(conn)
    ctConnections.append(ndict)

#! Corticoreticular
#! ----------------

#! In this case, there is only a single connection, so we write the
#! dictionary itself; it is very similar to the corThalBase, and to 
#! show that, we copy first, then update. We need no ``targets`` entry,
#! since Rp has only one neuron per location.
corRet = corThalBase.copy()
corRet.update({"sources": {"model": "L56pyr"}, "weights": 2.5})


#! Build all connections beginning in cortex
#! -----------------------------------------

#! Cortico-cortical, same orientation
print ccConnections
[topo.ConnectLayers(Vp_h, Vp_h, conn) for conn in ccConnections]
[topo.ConnectLayers(Vp_v, Vp_v, conn) for conn in ccConnections]

#! Cortico-cortical, cross-orientation
[topo.ConnectLayers(Vp_h, Vp_v, conn) for conn in ccxConnections]
[topo.ConnectLayers(Vp_v, Vp_h, conn) for conn in ccxConnections]

#! Cortico-thalamic connections
[topo.ConnectLayers(Vp_h, Tp, conn) for conn in ctConnections]
[topo.ConnectLayers(Vp_v, Tp, conn) for conn in ctConnections]
topo.ConnectLayers(Vp_h, Rp, corRet) 
topo.ConnectLayers(Vp_v, Rp, corRet) 

#! Thalamo-cortical connections
#! ----------------------------

#! **Note:** According to the text on p. 1674, bottom right, of 
#! the Hill & Tononi paper, thalamocortical connections are 
#! created by selecting from the thalamic population for each
#! L4 pyramidal cell, ie, are *convergent* connections.
#!
#! We first handle the rectangular thalamocortical connections.
thalCorRect = {"connection_type": "convergent",
               "sources": {"model": "TpRelay"},
               "synapse_model": "AMPA",
               "weights": 5.0,
               "delays": {"uniform": {"min": 2.75, "max": 3.25}}}

#! Horizontally tuned
thalCorRect.update({"mask": {"rectangular": {"lower_left" : [-4.0*dpc, -1.0*dpc],
                                             "upper_right": [ 4.0*dpc,  1.0*dpc]}}})
for conn in [{"targets": {"model": "L4pyr" }, "kernel": 0.5},
             {"targets": {"model": "L56pyr"}, "kernel": 0.3}]:
    thalCorRect.update(conn)
    topo.ConnectLayers(Tp, Vp_h, thalCorRect)

#! Vertically tuned
thalCorRect.update({"mask": {"rectangular": {"lower_left" : [-1.0*dpc, -4.0*dpc],
                                             "upper_right": [ 1.0*dpc,  4.0*dpc]}}})
for conn in [{"targets": {"model": "L4pyr" }, "kernel": 0.5},
             {"targets": {"model": "L56pyr"}, "kernel": 0.3}]:
    thalCorRect.update(conn)
    topo.ConnectLayers(Tp, Vp_v, thalCorRect)

#! Diffuse connections
thalCorDiff = {"connection_type": "convergent",
               "sources": {"model": "TpRelay"},
               "synapse_model": "AMPA",
               "weights": 5.0,
               "mask": {"circular": {"radius": 5.0 * dpc}},
               "kernel": {"gaussian": {"p_center": 0.1, "sigma": 7.5 * dpc}},
               "delays": {"uniform": {"min": 2.75, "max": 3.25}}}

for conn in [{"targets": {"model": "L4pyr" }},
             {"targets": {"model": "L56pyr"}}]:
    thalCorDiff.update(conn)
    topo.ConnectLayers(Tp, Vp_h, thalCorDiff)
    topo.ConnectLayers(Tp, Vp_v, thalCorDiff)

#! Thalamic connections
#! --------------------

#! Connections inside thalamus, including Rp
#!
#! *Note:* In Hill & Tononi, the inhibition between Rp cells is mediated by
#! GABA_B receptors. We use GABA_A receptors here to provide some self-dampening
#! of Rp.
#!
#! **Note:** The following code had a serious bug in v. 0.1: During the first
#! iteration of the loop, "synapse_model" and "weights" were set to "AMPA" and "0.1",
#! respectively and remained unchanged, so that all connections were created as 
#! excitatory connections, even though they should have been inhibitory. We now
#! specify synapse_model and weight explicitly for each connection to avoid this.

thalBase = {"connection_type": "divergent",
            "delays": {"uniform": {"min": 1.75, "max": 2.25}}}

for src, tgt, conn in [(Tp, Rp, {"sources": {"model": "TpRelay"},
                                 "synapse_model": "AMPA",
                                 "mask": {"circular": {"radius": 2.0 * dpc}},
                                 "kernel": {"gaussian": {"p_center": 1.0, "sigma": 7.5 * dpc}},
                                 "weights": 2.0}),
                       (Tp, Tp, {"sources": {"model": "TpInter"},
                                 "targets": {"model": "TpRelay"},
                                 "synapse_model": "GABA_A",
                                 "weights": -1.0,
                                 "mask": {"circular": {"radius": 2.0 * dpc}},
                                 "kernel": {"gaussian": {"p_center": 0.25, "sigma": 7.5 * dpc}}}),
                       (Tp, Tp, {"sources": {"model": "TpInter"},
                                 "targets": {"model": "TpInter"},
                                 "synapse_model": "GABA_A",
                                 "weights": -1.0,
                                 "mask": {"circular": {"radius": 2.0 * dpc}},
                                 "kernel": {"gaussian": {"p_center": 0.25, "sigma": 7.5 * dpc}}}),
                       (Rp, Tp, {"targets": {"model": "TpRelay"},
                                 "synapse_model": "GABA_A",
                                 "weights": -1.0,
                                 "mask": {"circular": {"radius": 12.0 * dpc}},
                                 "kernel": {"gaussian": {"p_center": 0.15, "sigma": 7.5 * dpc}}}),
                       (Rp, Tp, {"targets": {"model": "TpInter"},
                                 "synapse_model": "GABA_A",
                                 "weights": -1.0,
                                 "mask": {"circular": {"radius": 12.0 * dpc}},
                                 "kernel": {"gaussian": {"p_center": 0.15, "sigma": 7.5 * dpc}}}),
                       (Rp, Rp, {"targets": {"model": "RpNeuron"},
                                 "synapse_model": "GABA_A",
                                 "weights": -1.0,
                                 "mask": {"circular": {"radius": 12.0 * dpc}},
                                 "kernel": {"gaussian": {"p_center": 0.5, "sigma": 7.5 * dpc}}})]:
    thalBase.update(conn)
    topo.ConnectLayers(src, tgt, thalBase)

#! Thalamic input
#! --------------

#! Input to the thalamus from the retina.
#!
#! **Note:** Hill & Tononi specify a delay of 0 ms for this connection.
#! We use 1 ms here.
retThal = {"connection_type": "divergent",
           "synapse_model": "AMPA",
           "mask": {"circular": {"radius": 1.0 * dpc}},
           "kernel": {"gaussian": {"p_center": 0.75, "sigma": 2.5 * dpc}},
           "weights": 10.0,
           "delays": 1.0}

for conn in [{"targets": {"model": "TpRelay"}},
             {"targets": {"model": "TpInter"}}]:
    retThal.update(conn)
    topo.ConnectLayers(retina, Tp, retThal)


#! Checks on connections
#! ---------------------

#! As a very simple check on the connections created, we inspect
#! the connections from the central node of various layers. We
#! obtain all connections an plot the target neuron locations.

def connPlot(spop, smod, tmod, syn, titl):
    '''Plot connections.
       spop, smod  source population and model
       tmod        target model
       syn         synapse type
       titl        figure title'''

    # center location, cast to int since ``pyreport`` makes all div double
    ctr = [int(Params["N"]/2), int(Params["N"]/2)]

    # get element at center
    src = topo.GetElement(spop, ctr)

    # if center element is not individual neuron, find one
    # neuron of desired model in center element
    if nest.GetStatus(src, 'model')[0] == 'subnet':
        src = [n for n in nest.GetLeaves(src)[0] 
                 if nest.GetStatus([n],'model')[0]==smod][:1]

    # get position (in degrees) of chosen source neuron
    srcpos = topo.GetPosition(src)

    # get all targets
    tgts = nest.GetStatus(nest.FindConnections(src, synapse_type=syn), 'target')

    # pick targets of correct model type, get their positions,
    # convert list of (x,y) pairs to pair of x- and y-lists
    pos = zip(*[topo.GetPosition([n]) 
                for n in tgts if nest.GetStatus([n],
                                                'model')[0]==tmod])[0]

    # plot source neuron in red, slightly larger, targets on blue
    pylab.clf()
    pylab.plot(pos[0], pos[1], 'bo', markersize=5, zorder=99, label='Targets')
    #pylab.plot(srcpos[:1], srcpos[1:], 'ro', markersize=10, 
    #           markeredgecolor='r', zorder=1, label='Source')
    axsz = Params['visSize']/2 + 0.2
    pylab.axis([-axsz,axsz,-axsz,axsz])
    pylab.title(titl)
    pylab.legend()

    # pylab.show() required for `pyreport`
    pylab.show()

#! show Retina to TpRelay
connPlot(retina, 'Retina', 'TpRelay', 'AMPA', 'Connections Retina -> TpRelay')

#! show TpRelay to L4pyr
connPlot(Tp, 'TpRelay', 'L4pyr', 'AMPA', 'Connections TpRelay -> Vp L4pyr')

#! Recording devices
#! =================

#! This recording device setup is a bit makeshift. For each population
#! we want to record from, we create one ``multimeter``, then select
#! all nodes of the right model from the target population and
#! connect. ``loc`` is the subplot location for the layer.
recorders = {}
for name, loc, population, model in [('TpRelay'   , 1, Tp  , 'TpRelay'),
                                     ('Rp'        , 2, Rp  , 'RpNeuron'),
                                     ('Vp_v L4pyr', 3, Vp_v, 'L4pyr'),
                                     ('Vp_h L4pyr', 4, Vp_h, 'L4pyr')]:
    recorders[name] = (nest.Create('RecordingNode'), loc)
    tgts = [nd for nd in nest.GetLeaves(population)[0] 
            if nest.GetStatus([nd], 'model')[0]==model]
    nest.DivergentConnect(recorders[name][0], tgts) 

#! Example simulation
#! ====================

#! This simulation is set up to create a step-wise visualization of
#! the membrane potential. To do so, we simulate ``sim_interval``
#! milliseconds at a time, then read out data from the multimeters,
#! clear data from the multimeters and plot the data as pseudocolor
#! plots. 

#! show time during simulation
nest.SetStatus([0],{'print_time': True})

#! lower and upper limits for color scale, for each of the four
#! populations recorded.
vmn=[-80,-80,-80,-80]
vmx=[-50,-50,-50,-50]

nest.Simulate(Params['sim_interval'])

#! loop over simulation intervals
for t in pylab.arange(Params['sim_interval'], Params['simtime'], Params['sim_interval']):

    # do the simulation
    nest.Simulate(Params['sim_interval'])

    # clear figure and choose colormap
    pylab.clf()
    pylab.jet()

    # now plot data from each recorder in turn, assume four recorders
    for name, r in recorders.iteritems():
        rec = r[0]
        sp = r[1]
        pylab.subplot(2,2,sp)
        d = nest.GetStatus(rec)[0]['events']['V_m']

        if len(d) != Params['N']**2:
            # cortical layer with two neurons in each location, take average
            d = 0.5 * ( d[::2] + d[1::2] )

        # clear data from multimeter
        nest.SetStatus(rec, {'n_events': 0})
        pylab.imshow(pylab.reshape(d, (Params['N'],Params['N'])),
                     aspect='equal', interpolation='nearest',
                     extent=(0,Params['N']+1,0,Params['N']+1),
                     vmin=vmn[sp-1], vmax=vmx[sp-1])
        pylab.colorbar()
        pylab.title(name + ', t = %6.1f ms' % nest.GetKernelStatus()['time'])

    # required by ``pyreport``
    pylab.show()

#! just for some information at the end
print nest.GetKernelStatus()
