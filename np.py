from NeuroPy import NeuroPy
from pyeeg import *
import pyglet
npo=NeuroPy('/dev/ttyS25')
eegcoll = []

def npacb(attention_value):
	print attention_value
	return None

npo.setCallBack("attention",npacb)

npo.start()

i=1
while i<100:
	eegcoll.append(npo.rawValue)
	if(npo.meditation>50): 
		print npo.meditation, npo.rawValue,  npo.blinkStrength, npo.poorSignal, npo.midGamma, npo.lowGamma, npo.highBeta, npo.lowBeta, npo.highAlpha, npo.lowAlpha, npo.theta, npo.delta
		i+=1
		#song = pyglet.media.load('../SkyDrive/musicmix/18 - unknown 18 - Track 18 (1).ogg')
		#song.play()
		#pyglet.app.run()

print hurst(eegcoll[:])


##source libs
#git clone https://code.google.com/p/pyeeg/
#easy_install pyglet
#git clone git://github.com/lihas/NeuroPy
