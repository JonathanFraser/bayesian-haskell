import numpy as np 
import numpy.linalg as la
import matplotlib.pyplot as plt 


d = np.genfromtxt('test.csv', delimiter=',')
fig = plt.figure()
x = np.linspace(0,1,11)
true = 0.42545*(np.exp(x)-np.exp(-x))
bins = np.linspace(-0.1,20,50)
bincntr = (bins[1:]+bins[:-1])/2

#probs = d[:,0]
data = d[100000:,1:]
xsub = x[1:-1]
res = np.zeros((len(bincntr),len(xsub)))
for i in range(0,len(xsub)):
    (hst,_) = np.histogram(data[:,i],bins=bins,density=True)
    res[:,i]=hst
plt.contourf(xsub,bincntr,res)
plt.plot(x,true,'r')

plt.figure()
bins = np.linspace(-10,10,20)
bincntr = (bins[1:]+bins[:-1])/2
res = np.zeros((len(bincntr),len(xsub)))
padded = np.pad(data,[(0,0),(1,1)],'constant',constant_values=[(0,0),(0,1)])
right = padded[:,2:]
left = padded[:,:-2]

d2 = (right+left - 2*data)*100
err = d2-data
for i in range(0,len(xsub)):
    (hst,_) = np.histogram(err[:,i],bins=bins,density=True)
    res[:,i]=hst
plt.contourf(xsub,bincntr,res)

#plt.plot(x,test,'b')
plt.show()