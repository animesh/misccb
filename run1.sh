perl rul6.pl top5.txt 5 top5_train_n.txt top5_test_n.txt 1.3 0
perl rul6.pl top4.txt 4 top4_train_n.txt top4_test_n.txt 1.3 0

perl fuzzyrule_gen.pl ofs_top_5_tr_n_t.txt ofs_top_5_te_n_t.txt 5

perl fuzzy_c_means.pl ofs_top_5_tr_n_t.txt 5



 
