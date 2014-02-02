from NeuroPy import NeuroPy
import winsound 		 
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

npo=NeuroPy("COM26")

npo.start()
def npacb(sigval):
    print sigval
    return None

#npo.setCallBack("rawValue",npacb)
npo.setCallBack("attention",npacb)

freqeeg = 512

def crtfrearray(feeg):
    eegcoll = []
    cnteeg=0
    while cnteeg<feeg:
        eegcoll.append(npo.rawValue)
        cnteeg+=1
    freqs = np.fft.fftfreq(len(eegcoll))
    idx=np.argmax(np.abs(np.array(eegcoll))**2)
    freq=freqs[idx]
    freqhz=abs(freq*freqeeg)
    if freqhz>40 and freqhz<freqeeg:
        print freqhz
        winsound.Beep(int(freqhz),int(freqhz))
    return eegcoll

x=np.arange(0,freqeeg,1)
y=np.array(crtfrearray(freqeeg))

fig, axes = plt.subplots(nrows=2)

styles = ['r-', 'k-']
def plot(ax, style):
    return ax.plot(x, y, style, animated=True)[0]
lines = [plot(ax, style) for ax, style in zip(axes, styles)]

def animate(i):
    for j, line in enumerate(lines, start=1):
        line.set_ydata(np.fft.fft(np.array(crtfrearray(freqeeg)))*j)
    return lines


ani = animation.FuncAnimation(fig, animate, xrange(1, freqeeg), interval=1, blit=True)
plt.show()

#source
#c:\Python27\pythonw.exe c:\Users\animeshs\misccb\neuroplay.py
#http://stackoverflow.com/questions/3694918/how-to-extract-frequency-associated-with-fft-values-in-python
#http://stackoverflow.com/questions/8955869/why-is-plotting-with-matplotlib-so-slow

    
