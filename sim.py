import nest
import numpy as np
from NeuroTools import signals
nest.ResetKernel()
nest.SetKernelStatus({"resolution":.01})
nest.SetKernelStatus({"overwrite_files":True})
hh_neuron=nest.Create("hh_cond_exp_traub",n=1)
nest.SetStatus(hhneuron,{"I_e": 2000.})
voltmeter=nest.Create("voltmeter")
nest.SetStatus(voltmeter,{'to_file':True,'interval':.1})
nest.Connect(voltmeter,hh_neuron)
nest.Simulate(200.)
vm=np.loadtxt('voltmeter-4-0.dat')
plot(vm[:,1],vm[:,2])
gca().set_xlabel("Time(msec)")
gca().set_ylabel("Vm(mV)")
draw()

