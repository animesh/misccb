import nest
import nest.topology as topo
import math
import pylab
nest.ResetKernel()

conf = {
    'N'           : 10,
    'size'     : 8.0,
    'simtime'     : 500.0,
    'sim_interval': 5.0
    }

nest.CopyModel(
    'iaf_cond_alpha', 'Model',
    params = {
        'C_m'       :  32.0,
        'E_ex'      :   0.0,
        'V_reset'   : -60.0,
        'V_th'      : -40.0,
        't_ref'     :   2.0,
        'E_in'      : -80.0,
        'I_e'       :   0.0,
        'V_m'       : -65.0
        }
    )

nest.CopyModel(
    'Model', 'NCluster', 
    params = {
        'C_m'  :   16.0,
        'V_th' : -60.0,
        't_ref':   1.0,
        'E_in' : -70.0
        }
    )

layerProps = {
    'rows'     : conf['N'], 
    'columns'  : conf['N'],
    'extent'   : [conf['size'], conf['size']],
    'edge_wrap': True
}

layerProps.update({'elements': 'iaf_cond_alpha'})
Np = topo.CreateLayer(layerProps)

nest.CopyModel('Model', 'CtxExNeuron')

layerProps.update({'elements': 'iaf_psc_alpha'})
CEp = topo.CreateLayer(layerProps)

nest.CopyModel(
    'Model', 'CtxInNeuron', 
    params = {
        'C_m'  :   16.0,
        'V_th' : -60.0,
        't_ref':   1.0
        }
    )

layerProps.update({'elements': 'iaf_neuron'})
CIp = topo.CreateLayer(layerProps)
nest.PrintNetwork()

nest.CopyModel(
    'multimeter', 'RecordingNode',
    params = {
        'interval'   : conf['sim_interval'],
        'record_from': ['V_m'],
        'record_to'  : ['memory'],
        'withgid'    : True,
        'withpath'   : False,
        'withtime'   : False
        }
    )

populations = (Np, CEp, CIp)

Nppos = zip(*[topo.GetPosition([n]) for n in nest.GetLeaves(Np)[0]])[0]
pylab.plot(Nppos[0], Nppos[1], 'x')
axis = conf['size']/2 + 0.2
pylab.axis([-axis,axis,-axis,axis])
pylab.title('Layer Np')
pylab.show()



