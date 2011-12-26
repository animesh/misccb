1	1955	784224	fibroblast growth factor receptor 4                                                 
2	2186	208699	EST                                                                                 
3	1389	770394	Fc fragment of IgG, receptor, transporter, alpha                                    
4	742	812105	transmembrane protein                                                               
5	783	767183	hematopoietic cell-specific Lyn substrate 1                                         
6	820	770868	NGFI-A binding protein 2 (ERG1 binding protein 2)                                   
7	255	325182	cadherin 2, N-cadherin (neuronal)                                                   
8	1386	745019	EH domain containing 1                                                              
9	1916	80109	major histocompatibility complex, class II, DQ alpha 1                              
10	1626	811000	lectin, galactoside-binding, soluble, 3 binding protein (galectin 6 binding protein)
11	882	898237	HLA-B associated transcript-3                                                       
12	1003	796258	sarcoglycan, alpha (50kD dystrophin-associated glycoprotein)                        
13	2046	244618	ESTs                                                                                
14	1645	52076	olfactomedinrelated ER localized protein                                            
15	1207	143306	lymphocyte-specific protein 1                                                       
16	187	296448	insulin-like growth factor 2 (somatomedin A)                                        
17	1084	878652	postmeiotic segregation increased 2-like 12                                         
18	442	450152	NA                                                                                  
19	1954	814260	follicular lymphoma variant translocation 1                                         
20	229	68950	cyclin E1                                                                           

Training:

EWS	BL	NB	RMS
23	8	12	20

A	FGFR4
B	CDH2
C	EHD1
D	NAB2
E	FVT1
F	LSP1
G	AF1Q




1	7	255	325182	cadherin 2, N-cadherin (neuronal)			CDH2	                                                  
2	1	1955	784224	fibroblast growth factor receptor 4			FGFR4                                                 
3	8	1386	745019	EH domain containing 1					EHD1
4	15	1207	143306	lymphocyte-specific protein 1				LSP1
5	19	1954	814260	follicular lymphoma variant translocation 1		FVT1                                         
6	3	1389	770394	Fc fragment of IgG, receptor, transporter, alpha	
7	6	820	770868	NGFI-A binding protein 2 (ERG1 binding protein 2)	NAB2
8	4	742	812105	transmembrane protein					AF1Q


1954    784224  fibroblast growth factor receptor 4
2185    208699  EST
1388    770394  Fc fragment of IgG, receptor, transporter, alpha
743     812105  transmembrane protein
782     767183  hematopoietic cell-specific Lyn substrate 1
819     770868  NGFI-A binding protein 2 (ERG1 binding protein 2)
254     325182  cadherin 2, N-cadherin (neuronal)
1385    745019  EH domain containing 1
1915    80109           major histocompatibility complex, class II, DQ alpha
1
1625    811000  lectin, galactoside-binding, soluble, 3 binding protein
(galectin 6
881     binding protein)
1002    898237  HLA-B associated transcript-3
2045    796258  sarcoglycan, alpha (50kD dystrophin-associated glycoprotein)

1644    244618  ESTs
1206    52076           olfactomedinrelated ER localized protein
186     143306  lymphocyte-specific protein 1
1083    296448  insulin-like growth factor 2 (somatomedin A)
441     878652  postmeiotic segregation increased 2-like 12
1953    450152  NA
228     814260  follicular lymphoma variant translocation 1

But the way you selected was:
6       0       7       14      18      2       5       3       16      4
1       19      13      11      12      17      8       10      15      9
So I sorted my list based on this index and gave you the Top 7 from that.
The 20 Gene file has my original order and this 7 gene file has your order.

From 1:

7	1	8	15	19	3	6	4	17	5	2	20	14	12	13	18	9	11	16	10

Transpose:

1	7	255	325182	cadherin 2, N-cadherin (neuronal)			CDH2	                                                  
2	1	1955	784224	fibroblast growth factor receptor 4			FGFR4                                                 
3	8	1386	745019	EH domain containing 1					EHD1
4	15	1207	143306	lymphocyte-specific protein 1				LSP1
5	19	1954	814260	follicular lymphoma variant translocation 1		FVT1                                         
6	3	1389	770394	Fc fragment of IgG, receptor, transporter, alpha	
7	6	820	770868	NGFI-A binding protein 2 (ERG1 binding protein 2)	NAB2
8	4	742	812105	transmembrane protein					AF1Q
9	17
10	5
11	2
12	20
13	14
14	12
15	13
16	18
17	9
18	11
19	16
20	10





Training Sample
EWS	RMS	BL	NB	OTP	Sample
1	0	0	0	EWS	EWS-T1
1	0	0	0	EWS	EWS-T2
1	0	0	0	EWS	EWS-T3
1	0	0	0	EWS	EWS-T4
1	0	0	0	EWS	EWS-T6
1	0	0	0	EWS	EWS-T7
1	0	0	0	EWS	EWS-T9
1	0	0	0	EWS	EWS-T11
1	0	0	0	EWS	EWS-T12
1	0	0	0	EWS	EWS-T13
1	0	0	0	EWS	EWS-T14
1	0	0	0	EWS	EWS-T15
1	0	0	0	EWS	EWS-T19
1	0	0	0	EWS	EWS-C8
1	0	0	0	EWS	EWS-C3
1	0	0	0	EWS	EWS-C2
1	0	0	0	EWS	EWS-C4
1	0	0	0	EWS	EWS-C6
1	0	0	0	EWS	EWS-C9
1	0	0	0	EWS	EWS-C7
1	0	0	0	EWS	EWS-C1
1	0	0	0	EWS	EWS-C11
1	0	0	0	EWS	EWS-C10
0	0	1	0	BL	BL-C5
0	0	1	0	BL	BL-C6
0	0	1	0	BL	BL-C7
0	0	1	0	BL	BL-C8
0	0	1	0	BL	BL-C1
0	0	1	0	BL	BL-C2
0	0	1	0	BL	BL-C3
0	0	1	0	BL	BL-C4
0	0	0	1	NB	NB-C1
0	0	0	1	NB	NB-C2
0	0	0	1	NB	NB-C3
0	0	0	1	NB	NB-C6
0	0	0	1	NB	NB-C12
0	0	0	1	NB	NB-C7
0	0	0	1	NB	NB-C4
0	0	0	1	NB	NB-C5
0	0	0	1	NB	NB-C10
0	0	0	1	NB	NB-C11
0	0	0	1	NB	NB-C9
0	0	0	1	NB	NB-C8
0	1	0	0	RMS	RMS-C4
0	1	0	0	RMS	RMS-C3
0	1	0	0	RMS	RMS-C9
0	1	0	0	RMS	RMS-C2
0	1	0	0	RMS	RMS-C5
0	1	0	0	RMS	RMS-C6
0	1	0	0	RMS	RMS-C7
0	1	0	0	RMS	RMS-C8
0	1	0	0	RMS	RMS-C10
0	1	0	0	RMS	RMS-C11
0	1	0	0	RMS	RMS-T1
0	1	0	0	RMS	RMS-T4
0	1	0	0	RMS	RMS-T2
0	1	0	0	RMS	RMS-T6
0	1	0	0	RMS	RMS-T7
0	1	0	0	RMS	RMS-T8
0	1	0	0	RMS	RMS-T5
0	1	0	0	RMS	RMS-T3
0	1	0	0	RMS	RMS-T10
0	1	0	0	RMS	RMS-T11


Test Sample

EWS	RMS	BL	NB	OTP	Sample
1	0	0	0		TEST-9
1	0	0	0		TEST-11
1	0	0	0		TEST-5
0	0	0	1	NB	TEST-8
0	1	0	0	RMS	TEST-10
1	0	0	0		TEST-13
1	0	0	0		TEST-3
0	0	0	1	NB	TEST-1
1	0	0	0	EWS	TEST-2
0	1	0	0	RMS	TEST-4
0	0	1	0	BL	TEST-7
1	0	0	0	EWS	TEST-12
0	1	0	0	RMS	TEST-24
1	0	0	0	EWS	TEST-6
1	0	0	0	EWS	TEST-21
1	0	0	0	EWS	TEST-20
0	1	0	0	RMS	TEST-17
0	0	1	0	BL	TEST-18
0	1	0	0	RMS	TEST-22
0	0	0	1	NB	TEST-16
0	0	0	1	NB	TEST-23
0	0	0	1	NB	TEST-14
0	0	0	1	NB	TEST-25
0	0	1	0	BL	TEST-15
1	0	0	0	EWS	TEST-19



-----Original Message-----
From: Nikhil R. Pal [mailto:nrpal59@gmail.com]
Sent: Thursday, December 08, 2005 11:27 PM
To: sharma.animesh@gmail.com
- Hide quoted text -
Subject: Re: data files

On 12/8/05, Animesh Sharma <sharma.animesh@gmail.com> wrote:
> Nikhil da the genes have to be looked within their range otherwise it will
> not make any sense biologically. Reason is that highly expressed genes
such
> as Homeobox genes will alter the range for everyother gene. So expression
> has to be scaled within the context of gene.

So if is scale each gene between 0-1  with its own max-min, then it
would be easy to visualize, Right?


Now the list that you have given now and the list you gave earlier
seems to have same entries but different orders. Why? I used the file
of 20 that you sent and from there I selected seven.  I am confused to
attach correct names to these sevn. So I wanted the names of the seven
genes in order as they appear in the file that you sent me with the
most latest matlab code. I belive you have used the same seven that i
selected. I shall compare this with the file that I used for training
etc, so that we do not make any mistake.

> Coming to the List of Genes:
>
>        # from 0        Image ID        Name of the Gene
> 0       2185    208699  EST
> 1       881     binding protein)
> 2       819     770868  NGFI-A binding protein 2 (ERG1 binding protein 2)
> 3       1385    745019  EH domain containing 1
> 4       1625    811000  lectin, galactoside-binding, soluble, 3 binding
> protein (galectin 6
> 5       254     325182  cadherin 2, N-cadherin (neuronal)
> 6       1954    784224  fibroblast growth factor receptor 4
> 7       1388    770394  Fc fragment of IgG, receptor, transporter, alpha
> 8       1083    296448  insulin-like growth factor 2 (somatomedin A)
> 9       228     814260  follicular lymphoma variant translocation 1
> 10      441     878652  postmeiotic segregation increased 2-like 12
> 11      1644    244618  ESTs
> 12      1206    52076           olfactomedinrelated ER localized protein
> 13      2045    796258  sarcoglycan, alpha (50kD dystrophin-associated
> glycoprotein)
> 14      743     812105  transmembrane protein
> 15      1953    450152  NA
> 16      1915    80109           major histocompatibility complex, class
II,
> DQ alpha 1
> 17      186     143306  lymphocyte-specific protein 1
> 18      782     767183  hematopoietic cell-specific Lyn substrate 1
> 19      1002    898237  HLA-B associated transcript-3

Nikhil da, I forgot to give the index number of the genes:

Gene Number [Starting from index 0 and not 1, as required by you]
1954
2185
1388
743
782
819
254
1385
1915
1625
881
1002
2045
1644
1206
186
1083
441
1953
228

Gene Number [Starting from index 1 and not 0]
1955
2186
1389
742
783
820
255
1386
1916
1626
882
1003
2046
1645
1207
187
1084
442
1954
229

Nikhil da that will be really good.
What we have as our top 15 feature is:
Image ID        Gene Name
784224  fibroblast growth factor receptor 4
208699  EST
770394  Fc fragment of IgG, receptor, transporter, alpha
812105  transmembrane protein
767183  hematopoietic cell-specific Lyn substrate 1
770868  NGFI-A binding protein 2 (ERG1 binding protein 2)
325182  cadherin 2, N-cadherin (neuronal)
745019  EH domain containing 1
80109   major histocompatibility complex, class II, DQ alpha 1
811000  lectin, galactoside-binding, soluble, 3 binding protein (galectin 6
binding protein)
898237  HLA-B associated transcript-3
796258  sarcoglycan, alpha (50kD dystrophin-associated glycoprotein)
244618  ESTs
52076   olfactomedinrelated ER localized protein
143306  lymphocyte-specific protein 1

In the above list, 1) '208699   EST', 2) '770868        NGFI-A binding
protein 2 (ERG1 binding protein 2)' and 3) '898237      HLA-B associated
transcript-3' do not figure out in any of the paper I have read so far. They
are neither present in the Khan's 96 gene list and Liu's gene list
(http://www.biomedcentral.com/1471-2105/6/67 paper published in
bioinformatics 2005 and attached gene list).
Hope this helps,
Do let me know if there is anything else I can do.
Regards,
Animesh

1953    0.973233
1954    0.945656
1979    0.872075
1600    0.82238
1388    0.741096
1206    0.715162
969     0.090434
1318    0.087085
1385    0.084532
2252    0.054189
1266    0.039055
332     0.033319
1326    0.032625
1371    0.029688
275     0.026027
1891    0.025654
741     0.024081
2049    0.022634
787     0.022198
1895    0.021897
2275    0.0216
1173    0.021194
1798    0.020486
544     0.019278
1083    0.018287
1092    0.018066
1436    0.017889
1048    0.017703
1010    0.016594
1229    0.016469
1426    0.016405
822     0.016374
936     0.016031
1291    0.016011
1803    0.015815
988     0.015424
574     0.015401
483     0.015343
293     0.015311
2088    0.015287
714     0.015005
687     0.015004
442     0.014917
2264    0.014828
1828    0.014773
1115    0.01473
1093    0.014679
1203    0.014628
1065    0.014606
975     0.014584
2229    0.014525
425     0.014397
2185    0.0142
1468    0.014198
1472    0.014085
228     0.014018
560     0.01398
1962    0.013974
1523    0.013938
606     0.013853
1308    0.013744
1029    0.013722
878     0.013568
782     0.013559
1915    0.013433
312     0.013401
1376    0.013401
1127    0.013234
1733    0.01319
181     0.013148
2039    0.013071
173     0.013004
88      0.012938
905     0.01286
1883    0.012852
2156    0.012758
1369    0.012675
1332    0.012625
2165    0.012458
1415    0.012427
443     0.012199
73      0.012191
1216    0.012164
1452    0.012145
254     0.012132
522     0.01206
790     0.012016
1364    0.011994
668     0.011963
1836    0.0119
1209    0.011881
570     0.01184
1913    0.011795
2040    0.011787
468     0.011613
670     0.01161
1923    0.011541
1881    0.011534
1045    0.011485
1067    0.01146
1485    0.011385
628     0.011344
2158    0.011342
1158    0.01134
1966    0.011334
531     0.01133
287     0.011261
335     0.011261
971     0.01126
529     0.011259
1607    0.011243
1594    0.01123
1007    0.011228
1157    0.011192
320     0.011157
1659    0.011053
1579    0.011002
2021    0.011002
1002    0.010941
1071    0.010937
1006    0.010934
1520    0.010929
2110    0.010924
589     0.0109
221     0.010798
1943    0.010789
1910    0.010748
152     0.010736
845     0.010715
263     0.010691
385     0.010668
1037    0.01066
1946    0.010659
503     0.010649
1739    0.010615
1931    0.010575
480     0.010566
696     0.010563
369     0.010551
80      0.010545
944     0.010536
377     0.010485
1963    0.010478
779     0.010465
1346    0.010405
278     0.01034
793     0.010333
997     0.010333
2134    0.010316
123     0.010309
246     0.010308
757     0.01027
557     0.010249
1688    0.010222
1866    0.010221
146     0.010219
1605    0.010202
880     0.010201
1837    0.010198
601     0.010196
617     0.010196
2200    0.010133
1797    0.01013
1774    0.010103
306     0.010101
508     0.010088
2127    0.010088
2149    0.010084
1094    0.010059
1352    0.010048
662     0.010023
282     0.010012
1893    0.010002
434     0.009933
1543    0.009931
2052    0.009931
1492    0.009909
715     0.009906
2005    0.009883
866     0.009881
955     0.009828
1540    0.009821
2004    0.009809
1626    0.009798
1666    0.009777
37      0.009772
1467    0.009754
631     0.009751
789     0.009743
1642    0.009743
1586    0.009727
208     0.009721
2       0.009715
1722    0.009712
1141    0.009694
624     0.00969
1131    0.009666
509     0.00964
104     0.009634
1008    0.00963
2221    0.009627
2105    0.00961
202     0.009599
2051    0.009593
1280    0.009591
1914    0.009591
665     0.009588
1501    0.009586
798     0.009582
1633    0.009577
52      0.009565
1024    0.009561
266     0.00956
1148    0.009559
565     0.009549
2098    0.009548
985     0.009546
366     0.009542
1849    0.009538
170     0.009536
827     0.009536
1019    0.009535
195     0.009531
604     0.009523
379     0.00952
444     0.009514
1022    0.009499
837     0.009493
2302    0.009485
712     0.009484
454     0.009476
1026    0.00947
777     0.009468
311     0.009467
2133    0.009459
2174    0.009438
1869    0.009431
498     0.009429
438     0.009426
392     0.009417
2045    0.009417
718     0.009414
325     0.009411
461     0.00941
2160    0.00941
1126    0.009403
1937    0.009401
2194    0.009379
821     0.009373
1306    0.009372
289     0.009368
2239    0.009365
967     0.009347
854     0.009346
1461    0.009346
1381    0.009339
858     0.009325
339     0.009313
1269    0.009302
1769    0.009294
363     0.009289
245     0.009286
1696    0.009276
963     0.009256
121     0.009255
511     0.009255
1081    0.009254
1274    0.009253
1170    0.009247
1279    0.009244
1535    0.009229
1824    0.009228
334     0.009216
1537    0.009202
1205    0.009188
2151    0.009184
281     0.009181
909     0.009168
2259    0.009164
235     0.009161
499     0.009161
2126    0.009159
1681    0.009149
1236    0.009127
1245    0.009111
1125    0.00911
421     0.009105
746     0.009102
1516    0.009093
14      0.009091
1438    0.009082
122     0.009079
1265    0.009079
585     0.009069
881     0.009068
1675    0.009065
1488    0.009037
492     0.009029
2283    0.009024
676     0.009008
1856    0.009002
460     0.008996
1293    0.008983
950     0.008962
2020    0.00896
877     0.008957
1404    0.008949
1948    0.008942

Gene#   Gene Name
1955    'Fibroblast growth factor receptor 4'
2186    EST
1389    'Fc fragment of IgG receptor transporter alpha'
742     'transmembrane protein'
783     'hematopoietic cell-specific Lyn substrate 1'
820     'NGFI-A binding protein 2 (ERG1 binding protein 2)'
255     'cadherin 2 N-cadherin (neuronal)'
1386    'EH domain containing 1'
1916    'major histocompatibility complex class II DQ alpha 1'
1626    'lectin galactoside-binding soluble 3 binding protein (galectin 6
binding protein)'
882     'HLA-B associated transcript-3'
1003    'sarcoglycan alpha (50kD dystrophin-associated glycoprotein)'
2046    EST
1645    'olfactomedinrelated ER localized protein'
1207    'lymphocyte-specific protein 1'
187     'insulin-like growth factor 2 (somatomedin A)'
1084    'postmeiotic segregation increased 2-like 12'
442     'Spot 442'
1954    'follicular lymphoma variant translocation 1'
229     'cyclin E1'

