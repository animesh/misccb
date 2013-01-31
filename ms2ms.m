%% Oribitrap RAW to MZXML conversion
uf-mzxml Obb01926.RAW > Obb01926.MZXML
uf-mzxml Obb01937.RAW > Obb01937.MZXML
 
%% read into matlab structure

%MG132_3 = mzxmlread('M:\RAW\Lars\130107_MG132_3_HCD.mzXML');
Obb01926=mzxmlread('M:\RAW\melfalan0hr\Obb01926.MZXML')
Obb01937=mzxmlread('M:\RAW\melfalan0hr\Obb01937.MZXML')

%% 

[MZs,Ys] = msheatmap(msppresample(mzxml2peaks(Obb01937,'level',1),5000))
plot(Ys,'r.')
%fh1 = msheatmap(MZs,Ys)
plot(MZs,'r.')
plot(Ys,'r.')

proteinpropplot('EKMRHF')
proteinplot('EKMRHF')