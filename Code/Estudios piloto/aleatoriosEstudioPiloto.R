#PROJECT: SELECCION OPTIMA DE GRUPOS DE DIFUSIÓN APLICADO AL MARKETING VIRAL
#AUTHOR: ELENA CERRATO HERNANDEZ & ALBERTO IBARRONDO LUIS
#DATE: 08/06/2016


random11 <- round(runif(1, min = 1, max = 120))
random12 <- round(runif(1, min = 1, max = 120))
random13 <- round(runif(1, min = 1, max = 120))
random14 <- round(runif(1, min = 1, max = 120))
random15 <- round(runif(1, min = 1, max = 120))

random21 <- round(runif(2, min = 1, max = 120))
random22 <- round(runif(2, min = 1, max = 120))
random23 <- round(runif(2, min = 1, max = 120))
random24 <- round(runif(2, min = 1, max = 120))
random25 <- round(runif(2, min = 1, max = 120))

random31 <- round(runif(3, min = 1, max = 120))
random32 <- round(runif(3, min = 1, max = 120))
random33 <- round(runif(3, min = 1, max = 120))
random34 <- round(runif(3, min = 1, max = 120))
random35 <- round(runif(3, min = 1, max = 120))

random41 <- round(runif(4, min = 1, max = 120))
random42 <- round(runif(4, min = 1, max = 120))
random43 <- round(runif(4, min = 1, max = 120))
random44 <- round(runif(4, min = 1, max = 120))
random45 <- round(runif(4, min = 1, max = 120))

random51 <- round(runif(5, min = 1, max = 120))
random52 <- round(runif(5, min = 1, max = 120))
random53 <- round(runif(5, min = 1, max = 120))
random54 <- round(runif(5, min = 1, max = 120))
random55 <- round(runif(5, min = 1, max = 120))

random61 <- round(runif(6, min = 1, max = 120))
random62 <- round(runif(6, min = 1, max = 120))
random63 <- round(runif(6, min = 1, max = 120))
random64 <- round(runif(6, min = 1, max = 120))
random65 <- round(runif(6, min = 1, max = 120))

random71 <- round(runif(7, min = 1, max = 120))
random72 <- round(runif(7, min = 1, max = 120))
random73 <- round(runif(7, min = 1, max = 120))
random74 <- round(runif(7, min = 1, max = 120))
random75 <- round(runif(7, min = 1, max = 120))

random81 <- round(runif(8, min = 1, max = 120))
random82 <- round(runif(8, min = 1, max = 120))
random83 <- round(runif(8, min = 1, max = 120))
random84 <- round(runif(8, min = 1, max = 120))
random85 <- round(runif(8, min = 1, max = 120))

random91 <- round(runif(9, min = 1, max = 120))
random92 <- round(runif(9, min = 1, max = 120))
random93 <- round(runif(9, min = 1, max = 120))
random94 <- round(runif(9, min = 1, max = 120))
random95 <- round(runif(9, min = 1, max = 120))

random101 <- round(runif(10, min = 1, max = 120))
random102 <- round(runif(10, min = 1, max = 120))
random103 <- round(runif(10, min = 1, max = 120))
random104 <- round(runif(10, min = 1, max = 120))
random105 <- round(runif(10, min = 1, max = 120))

randomFri11 <- V(gFriends)$name[random11]
randomFri12 <- V(gFriends)$name[random12]
randomFri13 <- V(gFriends)$name[random13]
randomFri14 <- V(gFriends)$name[random14]
randomFri15 <- V(gFriends)$name[random15]

randomFri21 <- V(gFriends)$name[random21]
randomFri22 <- V(gFriends)$name[random22]
randomFri23 <- V(gFriends)$name[random23]
randomFri24 <- V(gFriends)$name[random24]
randomFri25 <- V(gFriends)$name[random25]

randomFri31 <- V(gFriends)$name[random31]
randomFri32 <- V(gFriends)$name[random32]
randomFri33 <- V(gFriends)$name[random33]
randomFri34 <- V(gFriends)$name[random34]
randomFri35 <- V(gFriends)$name[random35]

randomFri41 <- V(gFriends)$name[random41]
randomFri42 <- V(gFriends)$name[random42]
randomFri43 <- V(gFriends)$name[random43]
randomFri44 <- V(gFriends)$name[random44]
randomFri45 <- V(gFriends)$name[random45]

randomFri51 <- V(gFriends)$name[random51]
randomFri52 <- V(gFriends)$name[random52]
randomFri53 <- V(gFriends)$name[random53]
randomFri54 <- V(gFriends)$name[random54]
randomFri55 <- V(gFriends)$name[random55]

randomFri61 <- V(gFriends)$name[random61]
randomFri62 <- V(gFriends)$name[random62]
randomFri63 <- V(gFriends)$name[random63]
randomFri64 <- V(gFriends)$name[random64]
randomFri65 <- V(gFriends)$name[random65]

randomFri71 <- V(gFriends)$name[random71]
randomFri72 <- V(gFriends)$name[random72]
randomFri73 <- V(gFriends)$name[random73]
randomFri74 <- V(gFriends)$name[random74]
randomFri75 <- V(gFriends)$name[random75]

randomFri81 <- V(gFriends)$name[random81]
randomFri82 <- V(gFriends)$name[random82]
randomFri83 <- V(gFriends)$name[random83]
randomFri84 <- V(gFriends)$name[random84]
randomFri85 <- V(gFriends)$name[random85]

randomFri91 <- V(gFriends)$name[random91]
randomFri92 <- V(gFriends)$name[random92]
randomFri93 <- V(gFriends)$name[random93]
randomFri94 <- V(gFriends)$name[random94]
randomFri95 <- V(gFriends)$name[random95]

randomFri101 <- V(gFriends)$name[random101]
randomFri102 <- V(gFriends)$name[random102]
randomFri103 <- V(gFriends)$name[random103]
randomFri104 <- V(gFriends)$name[random104]
randomFri105 <- V(gFriends)$name[random105]



randomB11 <- round(runif(1, min = 1, max = 105))
randomB12 <- round(runif(1, min = 1, max = 105))
randomB13 <- round(runif(1, min = 1, max = 105))
randomB14 <- round(runif(1, min = 1, max = 105))
randomB15 <- round(runif(1, min = 1, max = 105))

randomB21 <- round(runif(2, min = 1, max = 105))
randomB22 <- round(runif(2, min = 1, max = 105))
randomB23 <- round(runif(2, min = 1, max = 105))
randomB24 <- round(runif(2, min = 1, max = 105))
randomB25 <- round(runif(2, min = 1, max = 105))

randomB31 <- round(runif(3, min = 1, max = 105))
randomB32 <- round(runif(3, min = 1, max = 105))
randomB33 <- round(runif(3, min = 1, max = 105))
randomB34 <- round(runif(3, min = 1, max = 105))
randomB35 <- round(runif(3, min = 1, max = 105))

randomB41 <- round(runif(4, min = 1, max = 105))
randomB42 <- round(runif(4, min = 1, max = 105))
randomB43 <- round(runif(4, min = 1, max = 105))
randomB44 <- round(runif(4, min = 1, max = 105))
randomB45 <- round(runif(4, min = 1, max = 105))

randomB51 <- round(runif(5, min = 1, max = 105))
randomB52 <- round(runif(5, min = 1, max = 105))
randomB53 <- round(runif(5, min = 1, max = 105))
randomB54 <- round(runif(5, min = 1, max = 105))
randomB55 <- round(runif(5, min = 1, max = 105))

randomB61 <- round(runif(6, min = 1, max = 105))
randomB62 <- round(runif(6, min = 1, max = 105))
randomB63 <- round(runif(6, min = 1, max = 105))
randomB64 <- round(runif(6, min = 1, max = 105))
randomB65 <- round(runif(6, min = 1, max = 105))

randomB71 <- round(runif(7, min = 1, max = 105))
randomB72 <- round(runif(7, min = 1, max = 105))
randomB73 <- round(runif(7, min = 1, max = 105))
randomB74 <- round(runif(7, min = 1, max = 105))
randomB75 <- round(runif(7, min = 1, max = 105))

randomB81 <- round(runif(8, min = 1, max = 105))
randomB82 <- round(runif(8, min = 1, max = 105))
randomB83 <- round(runif(8, min = 1, max = 105))
randomB84 <- round(runif(8, min = 1, max = 105))
randomB85 <- round(runif(8, min = 1, max = 105))

randomB91 <- round(runif(9, min = 1, max = 105))
randomB92 <- round(runif(9, min = 1, max = 105))
randomB93 <- round(runif(9, min = 1, max = 105))
randomB94 <- round(runif(9, min = 1, max = 105))
randomB95 <- round(runif(9, min = 1, max = 105))

randomB101 <- round(runif(10, min = 1, max = 105))
randomB102 <- round(runif(10, min = 1, max = 105))
randomB103 <- round(runif(10, min = 1, max = 105))
randomB104 <- round(runif(10, min = 1, max = 105))
randomB105 <- round(runif(10, min = 1, max = 105))

randomBo11 <- V(gBooks)$name[randomB11]
randomBo12 <- V(gBooks)$name[randomB12]
randomBo13 <- V(gBooks)$name[randomB13]
randomBo14 <- V(gBooks)$name[randomB14]
randomBo15 <- V(gBooks)$name[randomB15]

randomBo21 <- V(gBooks)$name[randomB21]
randomBo22 <- V(gBooks)$name[randomB22]
randomBo23 <- V(gBooks)$name[randomB23]
randomBo24 <- V(gBooks)$name[randomB24]
randomBo25 <- V(gBooks)$name[randomB25]

randomBo31 <- V(gBooks)$name[randomB31]
randomBo32 <- V(gBooks)$name[randomB32]
randomBo33 <- V(gBooks)$name[randomB33]
randomBo34 <- V(gBooks)$name[randomB34]
randomBo35 <- V(gBooks)$name[randomB35]

randomBo41 <- V(gBooks)$name[randomB41]
randomBo42 <- V(gBooks)$name[randomB42]
randomBo43 <- V(gBooks)$name[randomB43]
randomBo44 <- V(gBooks)$name[randomB44]
randomBo45 <- V(gBooks)$name[randomB45]

randomBo51 <- V(gBooks)$name[randomB51]
randomBo52 <- V(gBooks)$name[randomB52]
randomBo53 <- V(gBooks)$name[randomB53]
randomBo54 <- V(gBooks)$name[randomB54]
randomBo55 <- V(gBooks)$name[randomB55]

randomBo61 <- V(gBooks)$name[randomB61]
randomBo62 <- V(gBooks)$name[randomB62]
randomBo63 <- V(gBooks)$name[randomB63]
randomBo64 <- V(gBooks)$name[randomB64]
randomBo65 <- V(gBooks)$name[randomB65]

randomBo71 <- V(gBooks)$name[randomB71]
randomBo72 <- V(gBooks)$name[randomB72]
randomBo73 <- V(gBooks)$name[randomB73]
randomBo74 <- V(gBooks)$name[randomB74]
randomBo75 <- V(gBooks)$name[randomB75]

randomBo81 <- V(gBooks)$name[randomB81]
randomBo82 <- V(gBooks)$name[randomB82]
randomBo83 <- V(gBooks)$name[randomB83]
randomBo84 <- V(gBooks)$name[randomB84]
randomBo85 <- V(gBooks)$name[randomB85]

randomBo91 <- V(gBooks)$name[randomB91]
randomBo92 <- V(gBooks)$name[randomB92]
randomBo93 <- V(gBooks)$name[randomB93]
randomBo94 <- V(gBooks)$name[randomB94]
randomBo95 <- V(gBooks)$name[randomB95]

randomBo101 <- V(gBooks)$name[randomB101]
randomBo102 <- V(gBooks)$name[randomB102]
randomBo103 <- V(gBooks)$name[randomB103]
randomBo104 <- V(gBooks)$name[randomB104]
randomBo105 <- V(gBooks)$name[randomB105]




randomD11 <- round(runif(1, min = 1, max = 62))
randomD12 <- round(runif(1, min = 1, max = 62))
randomD13 <- round(runif(1, min = 1, max = 62))
randomD14 <- round(runif(1, min = 1, max = 62))
randomD15 <- round(runif(1, min = 1, max = 62))

randomD21 <- round(runif(2, min = 1, max = 62))
randomD22 <- round(runif(2, min = 1, max = 62))
randomD23 <- round(runif(2, min = 1, max = 62))
randomD24 <- round(runif(2, min = 1, max = 62))
randomD25 <- round(runif(2, min = 1, max = 62))

randomD31 <- round(runif(3, min = 1, max = 62))
randomD32 <- round(runif(3, min = 1, max = 62))
randomD33 <- round(runif(3, min = 1, max = 62))
randomD34 <- round(runif(3, min = 1, max = 62))
randomD35 <- round(runif(3, min = 1, max = 62))

randomD41 <- round(runif(4, min = 1, max = 62))
randomD42 <- round(runif(4, min = 1, max = 62))
randomD43 <- round(runif(4, min = 1, max = 62))
randomD44 <- round(runif(4, min = 1, max = 62))
randomD45 <- round(runif(4, min = 1, max = 62))

randomD51 <- round(runif(5, min = 1, max = 62))
randomD52 <- round(runif(5, min = 1, max = 62))
randomD53 <- round(runif(5, min = 1, max = 62))
randomD54 <- round(runif(5, min = 1, max = 62))
randomD55 <- round(runif(5, min = 1, max = 62))

randomDo11 <- V(gDolphins)$name[randomD11]
randomDo12 <- V(gDolphins)$name[randomD12]
randomDo13 <- V(gDolphins)$name[randomD13]
randomDo14 <- V(gDolphins)$name[randomD14]
randomDo15 <- V(gDolphins)$name[randomD15]

randomDo21 <- V(gDolphins)$name[randomD21]
randomDo22 <- V(gDolphins)$name[randomD22]
randomDo23 <- V(gDolphins)$name[randomD23]
randomDo24 <- V(gDolphins)$name[randomD24]
randomDo25 <- V(gDolphins)$name[randomD25]

randomDo31 <- V(gDolphins)$name[randomD31]
randomDo32 <- V(gDolphins)$name[randomD32]
randomDo33 <- V(gDolphins)$name[randomD33]
randomDo34 <- V(gDolphins)$name[randomD34]
randomDo35 <- V(gDolphins)$name[randomD35]

randomDo41 <- V(gDolphins)$name[randomD41]
randomDo42 <- V(gDolphins)$name[randomD42]
randomDo43 <- V(gDolphins)$name[randomD43]
randomDo44 <- V(gDolphins)$name[randomD44]
randomDo45 <- V(gDolphins)$name[randomD45]

randomDo51 <- V(gDolphins)$name[randomD51]
randomDo52 <- V(gDolphins)$name[randomD52]
randomDo53 <- V(gDolphins)$name[randomD53]
randomDo54 <- V(gDolphins)$name[randomD54]
randomDo55 <- V(gDolphins)$name[randomD55]



randomF11 <- round(runif(1, min = 1, max = 81))
randomF12 <- round(runif(1, min = 1, max = 81))
randomF13 <- round(runif(1, min = 1, max = 81))
randomF14 <- round(runif(1, min = 1, max = 81))
randomF15 <- round(runif(1, min = 1, max = 81))

randomF21 <- round(runif(2, min = 1, max = 81))
randomF22 <- round(runif(2, min = 1, max = 81))
randomF23 <- round(runif(2, min = 1, max = 81))
randomF24 <- round(runif(2, min = 1, max = 81))
randomF25 <- round(runif(2, min = 1, max = 81))

randomF31 <- round(runif(3, min = 1, max = 81))
randomF32 <- round(runif(3, min = 1, max = 81))
randomF33 <- round(runif(3, min = 1, max = 81))
randomF34 <- round(runif(3, min = 1, max = 81))
randomF35 <- round(runif(3, min = 1, max = 81))

randomF41 <- round(runif(4, min = 1, max = 81))
randomF42 <- round(runif(4, min = 1, max = 81))
randomF43 <- round(runif(4, min = 1, max = 81))
randomF44 <- round(runif(4, min = 1, max = 81))
randomF45 <- round(runif(4, min = 1, max = 81))

randomF51 <- round(runif(5, min = 1, max = 81))
randomF52 <- round(runif(5, min = 1, max = 81))
randomF53 <- round(runif(5, min = 1, max = 81))
randomF54 <- round(runif(5, min = 1, max = 81))
randomF55 <- round(runif(5, min = 1, max = 81))




randomrg11 <- round(runif(10, min = 1, max = 200))
randomrg12 <- round(runif(10, min = 1, max = 200))
randomrg13 <- round(runif(10, min = 1, max = 200))
randomrg14 <- round(runif(10, min = 1, max = 200))
randomrg15 <- round(runif(10, min = 1, max = 200))

randomrg21 <- round(runif(10, min = 1, max = 200))
randomrg22 <- round(runif(10, min = 1, max = 200))
randomrg23 <- round(runif(10, min = 1, max = 200))
randomrg24 <- round(runif(10, min = 1, max = 200))
randomrg25 <- round(runif(10, min = 1, max = 200))

randomrg31 <- round(runif(10, min = 1, max = 200))
randomrg32 <- round(runif(10, min = 1, max = 200))
randomrg33 <- round(runif(10, min = 1, max = 200))
randomrg34 <- round(runif(10, min = 1, max = 200))
randomrg35 <- round(runif(10, min = 1, max = 200))

randomrg41 <- round(runif(15, min = 1, max = 500))
randomrg42 <- round(runif(15, min = 1, max = 500))
randomrg43 <- round(runif(15, min = 1, max = 500))
randomrg44 <- round(runif(15, min = 1, max = 500))
randomrg45 <- round(runif(15, min = 1, max = 500))

randomrg51 <- round(runif(15, min = 1, max = 500))
randomrg52 <- round(runif(15, min = 1, max = 500))
randomrg53 <- round(runif(15, min = 1, max = 500))
randomrg54 <- round(runif(15, min = 1, max = 500))
randomrg55 <- round(runif(15, min = 1, max = 500))

randomrg61 <- round(runif(15, min = 1, max = 500))
randomrg62 <- round(runif(15, min = 1, max = 500))
randomrg63 <- round(runif(15, min = 1, max = 500))
randomrg64 <- round(runif(15, min = 1, max = 500))
randomrg65 <- round(runif(15, min = 1, max = 500))

randomrg71 <- round(runif(20, min = 1, max = 1000))
randomrg72 <- round(runif(20, min = 1, max = 1000))
randomrg73 <- round(runif(20, min = 1, max = 1000))
randomrg74 <- round(runif(20, min = 1, max = 1000))
randomrg75 <- round(runif(20, min = 1, max = 1000))

randomrg81 <- round(runif(20, min = 1, max = 1000))
randomrg82 <- round(runif(20, min = 1, max = 1000))
randomrg83 <- round(runif(20, min = 1, max = 1000))
randomrg84 <- round(runif(20, min = 1, max = 1000))
randomrg85 <- round(runif(20, min = 1, max = 1000))

randomrg91 <- round(runif(20, min = 1, max = 5000))
randomrg92 <- round(runif(20, min = 1, max = 5000))
randomrg93 <- round(runif(20, min = 1, max = 5000))
randomrg94 <- round(runif(20, min = 1, max = 5000))
randomrg95 <- round(runif(20, min = 1, max = 5000))

randomrg101 <- round(runif(20, min = 1, max = 10000))
randomrg102 <- round(runif(20, min = 1, max = 10000))
randomrg103 <- round(runif(20, min = 1, max = 10000))
randomrg104 <- round(runif(20, min = 1, max = 10000))
randomrg105 <- round(runif(20, min = 1, max = 10000))