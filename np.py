from NeuroPy import NeuroPy
object1=NeuroPy("COM26","57600") #If port not given 57600 is automatically assume #object1=NeuroPy("/dev/rfcomm0") for linux

def attention_callback(attention_value):
	print "Value of attention is",attention_value
	return None

object1.setCallBack("attention",attention_callback)

object1.start()

while True:
	if(object1.meditation>70): #another way of accessing data provided by headset (1st being call backs)
		object1.stop()         #if meditation level reaches above 70, stop fetching data from the headset
