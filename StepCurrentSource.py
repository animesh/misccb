"""
Simple test of injecting time-varying current into a cell

Andrew Davison, UNIC, CNRS
May 2009

$Id: StepCurrentSource.py 647 2009-06-09 12:37:02Z apdavison $
"""

from pyNN.utility import get_script_args

simulator_name = get_script_args(1)[0]  
exec("from pyNN.%s import *" % simulator_name)

setup()

cell = create(IF_curr_exp, {'v_thresh': -55.0, 'tau_refrac': 5.0})
current_source = StepCurrentSource([50.0, 110.0, 150.0, 210.0],
                                   [0.4, 0.6, -0.2, 0.2])
cell.inject(current_source)

record_v(cell, "Results/StepCurrentSource_%s.v" % simulator_name)
run(250.0)
  
end()
