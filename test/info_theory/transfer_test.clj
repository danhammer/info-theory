(ns info-theory.transfer-test
  (:use [midje sweet]
        info-theory.transfer))

(def x [1 1 1 0 1 1 0 1 0 0 1 1 0 0 1 1 1 0 1 1 0 1])
(def y [1 0 1 0 1 0 1 0 1 1 0 1 1 0 1 1 0 1 1 0 0 1])

(facts
  "check that y provides more information toward x than vice versa"
  (info-map 1 x y) => (roughly 0.234331)
  (info-map 1 y x) => (roughly 0.004771))

(def prop
  (bin-vector 10
   [0.08743169 0.09600000 0.10078740 0.10299870 0.11336032 0.10989011
    0.11253197 0.09154930 0.09636364 0.06972789 0.07090909 0.07179487
    0.09803922 0.09350238 0.06572770 0.07611940 0.11755725 0.10815047
    0.05479452 0.06004619 0.08172043 0.07317073 0.07462687 0.08926261
    0.10706150 0.11123348 0.10930233 0.08865248 0.10828025 0.10864745
    0.07289294 0.07734807 0.07368421 0.04656863 0.05339806 0.06485356
    0.05419580 0.09752322 0.07434053 0.07601713 0.07525870 0.08023256
    0.08677686 0.06779661 0.08588957 0.07636364 0.08250825 0.06949153
    0.07446809 0.07779886 0.05916031 0.08163265 0.07977737 0.07017544
    0.07025761 0.06997743 0.09273183 0.09443099 0.06835443 0.10937500
    0.05168986 0.07991803 0.06597938 0.07453416 0.05069124 0.06474820
    0.04679803 0.06872038 0.05991736 0.07526882 0.07400380 0.08867925
    0.05956679 0.07894737 0.09615385 0.08532934 0.08371385 0.05040650
    0.07885906 0.10613599 0.08992248 0.05685131 0.04341927 0.09179927
    0.08780992 0.06281662 0.07916667 0.08323699 0.07182320 0.06547619
    0.06842924 0.05683564 0.07488987 0.07611940 0.09010340 0.08913649
    0.07362784 0.08217270 0.07264297 0.07612457 0.06578947 0.05436893
    0.07168459 0.08416548 0.06285714 0.05635649 0.07486034 0.07497467
    0.07677725]))

(def price
  (bin-vector 10
   [905.2205 1003.0581 1112.6675 1105.1136 1089.8712 1084.3982 1083.3080
    1084.3561 1089.0836 1071.6768 1030.9363 1015.3650 1045.4308  852.6602
    782.4115  794.1126  689.5821  500.2438  486.4000  491.0290  464.4026
    439.4968  444.0458  475.6299  529.3145  586.8671  639.1939  678.3224
    696.2799  687.2295  730.7181  785.7633  718.2630  694.2384  628.6275
    596.9411  619.7168  631.8858  636.2282  636.3899  636.7188  637.3054
    665.8270  705.8145  728.5392  727.7312  733.1096  743.1197  755.5672
    781.4480  794.9787  795.1054  799.5101  794.7205  781.8656  768.7110
    763.4030  772.5778  773.1577  799.8937  850.8898  884.0780  883.0871
    934.2138  939.8496 1059.0100 1067.3718 1178.4057 1166.6664 1171.7092
    1206.0928 1251.2696 1228.5927 1131.9755 1138.6653 1122.1060 1119.6331
    1097.4568 1077.5465 1079.3114 1045.8254 1031.1746 1044.0567 1050.5491
    1002.3669  994.5199  997.0306  990.0464  985.7700  984.3943  967.5919
    973.8800  999.9350 1035.4789 1047.1728 1056.7749 1112.6341 1093.9431
    1055.7929 1029.8885 1039.1532  976.0174  919.4029  936.3120  934.9649
    930.0994  932.7393  897.8705  829]))
