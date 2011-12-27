import unittest

from nest.tests import test_errors
from nest.tests import test_stack
from nest.tests import test_create
from nest.tests import test_status
from nest.tests import test_connectapi
from nest.tests import test_findconnections
from nest.tests import test_connectoptions
from nest.tests import test_events
from nest.tests import test_networks
from nest.tests import test_threads

def suite():

    import nest.tests

    suite = unittest.TestSuite()

    suite.addTest(test_errors.suite())
    suite.addTest(test_stack.suite())
    suite.addTest(test_create.suite())                    
    suite.addTest(test_status.suite())
    suite.addTest(test_connectapi.suite())
    suite.addTest(test_findconnections.suite())    
    suite.addTest(test_connectoptions.suite())    
    suite.addTest(test_events.suite())
    suite.addTest(test_networks.suite())
    suite.addTest(test_threads.suite())    
    
    return suite


if __name__ == "__main__":

    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite())
