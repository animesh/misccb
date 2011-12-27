import nest
import numpy as np
#from NeuroTools import signals
import matplotlib.pyplot as plt

nest.ResetKernel()
nest.SetKernelStatus({"resolution":.01})
nest.SetKernelStatus({"overwrite_files":True})
hhneuron=nest.Create("hh_cond_exp_traub",n=1)
nest.SetStatus(hhneuron,{"I_e": 2000.})
voltmeter=nest.Create("voltmeter")
nest.SetStatus(voltmeter,{'to_file':True,'interval':.1})
nest.Connect(voltmeter,hhneuron)
nest.Simulate(200.)

fig = plt.figure()
vm=np.loadtxt('voltmeter-2-0.dat')
gca = fig.add_subplot(111)
gca.plot(vm[:,1],vm[:,2])
gca.set_xlabel("Time(msec)")
gca.set_ylabel("Vm(mV)")
plt.show()

