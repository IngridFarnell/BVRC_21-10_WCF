# Calculating biomass from live and dead wood
# Alana Clason, Ingrid Farnell
#Feb 18, 2022


##################### Tree carbon (kg/tree) ############################
# Biomass is calculated using Ung et al. 2008 allometric equations. biomass is in kg. 

# Standing dead (SD) A decay class and species specificstructural reduction
# factor is applied to each of the components of the allometric equation i.e. bark, bole, top. This accounts for the loss 
# of biomass through decay (Domke et al. 2011). A species and decay class density reduction factor is applied, this 
# accounts for the density loss through decay (Harmon et al. 2011).

# SDCarbon (kg) = BIOMASS x SRF x DCRF
# SDCarbon (kg) = ((Ywood*SRF) + (Ybark*SRF) + (Yfoliage*SRF) + (Ybranches*SRF)) X DCRF
TreeBiomassFN <- function(Species,DBH,HT,Tree_class){
  if(is.na(Species)){
    print(paste("Species is not found"))
    Sp_B <- NA
  } else if(Species=="At"){
     if(Tree_class < 3 ){
      Sp_B <-((0.0143*DBH^1.9369*HT^1.0579)+(0.0063*DBH^2.0744*HT^0.6691)+
                (0.0150*DBH^2.9068*HT^-0.6306)+(0.0284*DBH^1.6020))
      } else if (Tree_class == 3){
        Sp_B <-(((0.0143*DBH^1.9369*HT^1.0579)*1)+((0.0063*DBH^2.0744*HT^0.6691)*0.92)+
                        ((0.0150*DBH^2.9068*HT^-0.6306)*1))*0.97
      } else if (Tree_class == 4){
        Sp_B <-(((0.0143*DBH^1.9369*HT^1.0579)*1)+((0.0063*DBH^2.0744*HT^0.6691)*0.66)+
                        ((0.0150*DBH^2.9068*HT^-0.6306)*0.5))*0.75
      } else if(Tree_class == 5){
        Sp_B <-(((0.0143*DBH^1.9369*HT^1.0579)*1)+((0.0063*DBH^2.0744*HT^0.6691)*0.39)+
                        ((0.0150*DBH^2.9068*HT^-0.6306)*0.2))*0.868
      } else if(Tree_class == 6){
        Sp_B <-(((0.0143*DBH^1.9369*HT^1.0579)*1)+((0.0063*DBH^2.0744*HT^0.6691)*0.21)+
                        ((0.0150*DBH^2.9068*HT^-0.6306)*0.1))*0.613
      } else if(Tree_class == 7){
        Sp_B <-(((0.0143*DBH^1.9369*HT^1.0579)*1)+((0.0063*DBH^2.0744*HT^0.6691)*0)+
                        ((0.0150*DBH^2.9068*HT^-0.6306)*0))*0.613
      } else if(Tree_class == 8){
        Sp_B <-(((0.0143*DBH^1.9369*HT^1.0579)*1)+((0.0063*DBH^2.0744*HT^0.6691)*0)+
                        ((0.0150*DBH^2.9068*HT^-0.6306)*0))*0.613
      }
    
  } else if(Species=="Ac"){
      if(Tree_class < 3){
        Sp_B <-((0.0051*DBH^1.0697*HT^2.2748)+(0.0009*DBH^1.3061*HT^2.0109)+
                    (0.0131*DBH^2.5760)+(0.0224*DBH^1.8368))
      } else if (Tree_class == 3){
        Sp_B <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0.92)+
                      ((0.0131*DBH^2.5760)*1))*1.006
      } else if (Tree_class == 4){
        Sp_B <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0.66)+
                          ((0.0131*DBH^2.5760)*0.5))*0.793
      } else if (Tree_class == 5){
        Sp_B <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0.39)+
                        ((0.0131*DBH^2.5760)*0.2))*0.868
      } else if (Tree_class == 6){
        Sp_B <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0.21)+
                        ((0.0131*DBH^2.5760)*0.1))*0.613
      } else if (Tree_class == 7){
        Sp_B <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0)+
                        ((0.0131*DBH^2.5760)*0))*0.613
      } else if (Tree_class == 8){
        Sp_B <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0)+
                        ((0.0131*DBH^2.5760)*0))*0.613
      }
  } else if(Species=="Cw"){
      if(Tree_class < 3){
        Sp_B <-((0.0188*DBH^1.3376*HT^1.5293)+(0.0002*DBH^2.4369*HT^1.1315)+
                  (0.0611*DBH^1.9208)+(0.1097*DBH^1.5530))
      } else if (Tree_class == 3){
        Sp_B <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0.92)+
                 ((0.0131*DBH^2.5760)*1))*1.040
      } else if (Tree_class == 4){
        Sp_B <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0.66)+
                  ((0.0131*DBH^2.5760)*0.5))*0.960
      } else if (Tree_class == 5){
        Sp_B <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0.39)+
                  ((0.0131*DBH^2.5760)*0.2))*1.064
      } else if (Tree_class == 6){
        Sp_B <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0.21)+
                 ((0.0131*DBH^2.5760)*0.1))*0.656
      } else if (Tree_class == 7){
        Sp_B <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0)+
                 ((0.0131*DBH^2.5760)*0))*0.656
      } else if (Tree_class == 8){
        Sp_B <-(((0.0051*DBH^1.0697*HT^2.2748)*1)+((0.0009*DBH^1.3061*HT^2.0109)*0)+
                  ((0.0131*DBH^2.5760)*0))*0.656
      }
  } else if(Species=="Bl"){
      if(Tree_class < 3){
        Sp_B <-((0.0220*DBH^1.6469*HT^1.1714)+(0.0061*DBH^1.8603*HT^0.7693)+
                  (0.0265*DBH^3.6747*HT^-1.5958)+(0.0509*DBH^2.9909*HT^-1.2271))
      } else if (Tree_class == 3){
        Sp_B <-(((0.0220*DBH^1.6469*HT^1.1714)*1)+((0.0061*DBH^1.8603*HT^0.7693)*0.92)+
                        ((0.0265*DBH^3.6747*HT^-1.5958)*1))*1.04
      } else if (Tree_class == 4){
        Sp_B <-(((0.0220*DBH^1.6469*HT^1.1714)*1)+((0.0061*DBH^1.8603*HT^0.7693)*0.66)+
                        ((0.0265*DBH^3.6747*HT^-1.5958)*0.5))*1.068
      } else if (Tree_class == 5){
        Sp_B <-(((0.0220*DBH^1.6469*HT^1.1714)*1)+((0.0061*DBH^1.8603*HT^0.7693)*0.39)+
                        ((0.0265*DBH^3.6747*HT^-1.5958)*0.2))*1
      } else if (Tree_class == 6){
        Sp_B <-(((0.0220*DBH^1.6469*HT^1.1714)*1)+((0.0061*DBH^1.8603*HT^0.7693)*0.21)+
                        ((0.0265*DBH^3.6747*HT^-1.5958)*0.1))*0.696
      } else if (Tree_class == 7){
        Sp_B <-(((0.0220*DBH^1.6469*HT^1.1714)*1)+((0.0061*DBH^1.8603*HT^0.7693)*0)+
                        ((0.0265*DBH^3.6747*HT^-1.5958)*0))*0.696
      } else if (Tree_class == 8){
        Sp_B <-(((0.0220*DBH^1.6469*HT^1.1714)*1)+((0.0061*DBH^1.8603*HT^0.7693)*0)+
                        ((0.0265*DBH^3.6747*HT^-1.5958)*0))*0.696
      }  
  } else if(Species=="Ep"){
      if(Tree_class < 3){
          Sp_B <-((0.0333*DBH^2.0794*HT^0.6811)+(0.0079*DBH^1.9905*HT^0.6553)+
                    (0.0253*DBH^3.1518*HT^-0.9083)+(0.1361*DBH^2.2978*HT^-1.0934))
      } else if(Tree_class == 3){
        Sp_B <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0.92)+
                    ((0.0253*DBH^3.1518*HT^-0.9083)*1))*1.016
      } else if(Tree_class == 4){
        Sp_B <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0.66)+
                    ((0.0253*DBH^3.1518*HT^-0.9083)*0.5))*0.713
      } else if(Tree_class == 5) {
        Sp_B <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0.39)+
                        ((0.0253*DBH^3.1518*HT^-0.9083)*0.2))*0.777
      } else if(Tree_class == 6) {
        Sp_B <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0.21)+
                        ((0.0253*DBH^3.1518*HT^-0.9083)*0.1))*0.439
      } else if(Tree_class == 7) {
        Sp_B <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0)+
                        ((0.0253*DBH^3.1518*HT^-0.9083)*0))*0.439
      } else if(Tree_class == 8) {
        Sp_B <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0)+
                        ((0.0253*DBH^3.1518*HT^-0.9083)*0))*0.439
      }
  } else if(Species=="Hw"){
    if(Tree_class < 3){
      Sp_B <-((0.0113*DBH^1.9332*HT^1.1125)+(0.0019*DBH^2.3356*HT^0.6371)+
                (0.0609*DBH^2.0021)+(0.2656*DBH^2.0107*HT^-0.7963))
      } else if(Tree_class == 3){
        Sp_B <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0.92)+
                  ((0.0253*DBH^3.1518*HT^-0.9083)*1))*1.040
      } else if(Tree_class == 4) {
        Sp_B <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0.66)+
                  ((0.0253*DBH^3.1518*HT^-0.9083)*0.5))*1.080
      } else if(Tree_class == 5) {
        Sp_B <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0.39)+
                  ((0.0253*DBH^3.1518*HT^-0.9083)*0.2))*0.848
      } else if(Tree_class == 6) {
        Sp_B <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0.21)+
                  ((0.0253*DBH^3.1518*HT^-0.9083)*0.1))*0.525
      } else if(Tree_class == 7) {
        Sp_B <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0)+
                  ((0.0253*DBH^3.1518*HT^-0.9083)*0))*0.525
      } else if(Tree_class == 8) {
        Sp_B <-(((0.0333*DBH^2.0794*HT^0.6811)*1)+((0.0079*DBH^1.9905*HT^0.6553)*0)+
                  ((0.0253*DBH^3.1518*HT^-0.9083)*0))*0.525
      }
  } else if(Species=="Pl"){
    if(Tree_class < 3){
        Sp_B <-((0.0239*DBH^1.6827*HT^1.1878)+(0.0117*DBH^1.6398*HT^0.6524)+
                  (0.0285*DBH^3.3764*HT^-1.4395)+(0.0769*DBH^2.6834*HT^-1.2484))
      } else if(Tree_class == 3){
        Sp_B <-(((0.0239*DBH^1.6827*HT^1.1878)*1)+((0.0117*DBH^1.6398*HT^0.6524)*0.92)+
                        ((0.0285*DBH^3.3764*HT^-1.4395)*1))*0.98
      } else if(Tree_class == 4){
        Sp_B <-(((0.0239*DBH^1.6827*HT^1.1878)*1)+((0.0117*DBH^1.6398*HT^0.6524)*0.66)+
                        ((0.0285*DBH^3.3764*HT^-1.4395)*0.5))*1.04
      } else if(Tree_class == 5){
        Sp_B <-(((0.0239*DBH^1.6827*HT^1.1878)*1)+((0.0117*DBH^1.6398*HT^0.6524)*0.39)+
                        ((0.0285*DBH^3.3764*HT^-1.4395)*0.2))*1.02
      } else if(Tree_class == 6){
        Sp_B <-(((0.0239*DBH^1.6827*HT^1.1878)*1)+((0.0117*DBH^1.6398*HT^0.6524)*0.21)+
                        ((0.0285*DBH^3.3764*HT^-1.4395)*0.1))*0.727
      } else if(Tree_class == 7){
        Sp_B <-(((0.0239*DBH^1.6827*HT^1.1878)*1)+((0.0117*DBH^1.6398*HT^0.6524)*0)+
                        ((0.0285*DBH^3.3764*HT^-1.4395)*0))*0.727
      } else if(Tree_class == 8){
        Sp_B <-(((0.0239*DBH^1.6827*HT^1.1878)*1)+((0.0117*DBH^1.6398*HT^0.6524)*0)+
                        ((0.0285*DBH^3.3764*HT^-1.4395)*0))*0.727           
      }
  } else if(Species=="Sx"){
    if(Tree_class < 3){
      Sp_B <-((0.0133*DBH^1.3303*HT^1.6877)+(0.0086*DBH^1.6216*HT^0.8192)+
        (0.0428*DBH^2.7965*HT^-0.7328)+(0.0854*DBH^2.4388*HT^-0.7630))
      } else if(Tree_class == 3){  
        Sp_B <-(((0.0133*DBH^1.3303*HT^1.6877)*1)+((0.0086*DBH^1.6216*HT^0.8192)*0.92)+
                        ((0.0428*DBH^2.7965*HT^-0.7328)*1))*0.996
      } else if(Tree_class == 4){
        Sp_B <-(((0.0133*DBH^1.3303*HT^1.6877)*1)+((0.0086*DBH^1.6216*HT^0.8192)*0.66)+
                        ((0.0428*DBH^2.7965*HT^-0.7328)*0.5))*0.943
      } else if(Tree_class == 5){
        Sp_B <-(((0.0133*DBH^1.3303*HT^1.6877)*1)+((0.0086*DBH^1.6216*HT^0.8192)*0.39)+
                        ((0.0428*DBH^2.7965*HT^-0.7328)*0.2))*0.991
      } else if(Tree_class == 6){
        Sp_B <-(((0.0133*DBH^1.3303*HT^1.6877)*1)+((0.0086*DBH^1.6216*HT^0.8192)*0.21)+
                        ((0.0428*DBH^2.7965*HT^-0.7328)*0.1))*0.555
      } else if(Tree_class == 7){
        Sp_B <-(((0.0133*DBH^1.3303*HT^1.6877)*1)+((0.0086*DBH^1.6216*HT^0.8192)*0)+
                        ((0.0428*DBH^2.7965*HT^-0.7328)*0))*0.555
      } else if(Tree_class == 8){
        Sp_B <-(((0.0133*DBH^1.3303*HT^1.6877)*1)+((0.0086*DBH^1.6216*HT^0.8192)*0)+
                        ((0.0428*DBH^2.7965*HT^-0.7328)*0))*0.555
      }
  } else if(Species=="Fd"){
    if(Tree_class < 3){
      Sp_B <- ((0.0191*DBH^1.5365*HT^1.3634)+(0.0083*DBH^2.4811)+
        (0.0351*DBH^2.2421)+(0.0718*DBH^2.2935*HT^-0.4744))
      } else if(Tree_class == 3){
        Sp_B <- (((0.0191*DBH^1.5365*HT^1.3634)*1)+((0.0083*DBH^2.4811)*0.92)+
                         ((0.0351*DBH^2.2421)*1))*0.892
      } else if(Tree_class == 4){
        Sp_B <- (((0.0191*DBH^1.5365*HT^1.3634)*1)+((0.0083*DBH^2.4811)*0.66)+
                         ((0.0351*DBH^2.2421)*0.5))*0.831
      } else if(Tree_class == 5){
        Sp_B <- (((0.0191*DBH^1.5365*HT^1.3634)*1)+((0.0083*DBH^2.4811)*0.39)+
                         ((0.0351*DBH^2.2421)*0.2))*0.591
      } else if(Tree_class == 6){
        Sp_B <- (((0.0191*DBH^1.5365*HT^1.3634)*1)+((0.0083*DBH^2.4811)*0.21)+
                         ((0.0351*DBH^2.2421)*0.1))*0.433
      } else if(Tree_class == 7){
        Sp_B <- (((0.0191*DBH^1.5365*HT^1.3634)*1)+((0.0083*DBH^2.4811)*0)+
                         ((0.0351*DBH^2.2421)*0))*0.433
      } else if(Tree_class == 8){
        Sp_B <- (((0.0191*DBH^1.5365*HT^1.3634)*1)+((0.0083*DBH^2.4811)*0)+
                         ((0.0351*DBH^2.2421)*0))*0.433
      }
  } else if(Species=="UC"){
    if(Tree_class<3){
      Sp_B <-((0.0276*DBH^1.6868*HT^1.0953)+(0.0101*DBH^1.8486*HT^0.5525)+
                  (0.0313*DBH^2.9974*HT^-1.0383)+(0.1379*DBH^2.3981*HT^-1.0418))
      } else if(Tree_class == 3){
        Sp_B <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.92)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*1))*1.005
      } else if(Tree_class == 4){
        Sp_B <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.66)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*0.5))*1.017
      } else if(Tree_class == 5){
        Sp_B <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.39)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*0.2))*1.004
      } else if(Tree_class == 6){
        Sp_B <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.21)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*0.1))*0.659
      } else if(Tree_class == 7){
        Sp_B <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*0))*0.659
      } else if(Tree_class == 8){
        Sp_B <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*0))*0.659
      }
   } else if(Species=="Lw"){ #Using Unknown conifer
    if(Tree_class < 3){
      Sp_B <-((0.0276*DBH^1.6868*HT^1.0953)+(0.0101*DBH^1.8486*HT^0.5525)+
                    (0.0313*DBH^2.9974*HT^-1.0383)+(0.1379*DBH^2.3981*HT^-1.0418))
      } else if(Tree_class == 3){
        Sp_B <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.92)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*1))*1.005
      } else if(Tree_class == 4){
        Sp_B <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.66)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*0.5))*1.017
      } else if(Tree_class == 5){
        Sp_B <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.39)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*0.2))*1.004
      } else if(Tree_class == 6){
        Sp_B <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0.21)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*0.1))*0.659
      } else if(Tree_class == 7){
        Sp_B <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*0))*0.659
      } else if(Tree_class == 8){
        Sp_B <-(((0.0276*DBH^1.6868*HT^1.0953)*1)+((0.0101*DBH^1.8486*HT^0.5525)*0)+
                        ((0.0313*DBH^2.9974*HT^-1.0383)*0))*0.659
      }
  } else {
    print(paste("Species",Species,"not found"))
    Sp_B <- NA
    }
  return(Sp_B)
}

########################### Tree regeneration biomass ###########################################
#Regen biomass - Ung allometric equation
# Using the species allometric equation from Ung et al. (2008) - DBH and height are required so we are going to use a 
# very small DBH (0.1) and the mid-point height of the height class.  
# ywood = ??wood1*D^(??wood2)*H^(??wood3)
# ybark = ??bark1*D^(??bark2)*H^(??bark3)
# yfoliage = ??foliage1*D^(??foliage2)*H^(??foliage3)
# ytotal = ywood + ybark + ybranches
# where D is DBH (cm)
# H is height (m)

RegenBiomassFN_Ung <- function(Species, Height_class, Diam_est, Health){ # Live_Dead - should we specify live and dead? Is it worth it?
  if(is.na(Species)){
    print(paste("Species is not found"))
    Reg_B <- NA
  } else if(Species == "At"){ 
    if(Height_class == "0-30"){
      if(Health=="L"){
        Reg_B <- ((0.0143*Diam_est^1.9369*0.15^1.0579)+(0.0063*Diam_est^2.0744*0.15^0.6691)+
                     (0.0150*Diam_est^2.9068*0.15^-0.6306)+(0.0284*Diam_est^1.6020))
      } else if(Health=="D"){
        Reg_B <- ((0.0143*Diam_est^1.9369*0.15^1.0579)+(0.0063*Diam_est^2.0744*0.15^0.6691)+
                     (0.0150*Diam_est^2.9068*0.15^-0.6306)+(0.0284*Diam_est^1.6020))*0.95
      }
    } else if (Height_class == "31-130"){
      if(Health=="L"){
        Reg_B <- ((0.0143*Diam_est^1.9369*0.80^1.0579)+(0.0063*Diam_est^2.0744*0.80^0.6691)+
                   (0.0150*Diam_est^2.9068*0.80^-0.6306)+(0.0284*Diam_est^1.6020))
      } else if(Health=="D"){
        Reg_B <- ((0.0143*Diam_est^1.9369*0.80^1.0579)+(0.0063*Diam_est^2.0744*0.80^0.6691)+
                     (0.0150*Diam_est^2.9068*0.80^-0.6306)+(0.0284*Diam_est^1.6020))*0.95
      }
    }
  }else if(Species=="Ac"){ 
    if(Height_class == "0-30"){
      if(Health=="L"){
        Reg_B <- ((0.0051*Diam_est^1.0697*0.15^2.2748)+(0.0009*Diam_est^1.3061*0.15^2.0109)+
                   (0.0131*Diam_est^2.5760)+(0.0224*0.15^1.8368))
      } else if(Health=="D"){
        Reg_B <- ((0.0051*Diam_est^1.0697*0.15^2.2748)+(0.0009*Diam_est^1.3061*0.15^2.0109)+
                     (0.0131*Diam_est^2.5760)+(0.0224*0.15^1.8368))*0.95
      }
    } else if (Height_class == "31-130"){
      if(Health=="L"){
        Reg_B <- ((0.0051*Diam_est^1.0697*0.80^2.2748)+(0.0009*Diam_est^1.3061*0.80^2.0109)+
                     (0.0131*Diam_est^2.5760)+(0.0224*0.80^1.8368))
      }else if(Health=="D"){
        Reg_B <- ((0.0051*Diam_est^1.0697*0.80^2.2748)+(0.0009*Diam_est^1.3061*0.80^2.0109)+
                     (0.0131*Diam_est^2.5760)+(0.0224*0.80^1.8368))*0.95
      }
    }
  } else if(Species=="Cw"){ 
    if(Height_class == "0-30"){
      if(Health=="L"){
        Reg_B <- ((0.0188*Diam_est^1.3376*0.15^1.5293)+(0.0002*Diam_est^2.4369*0.15^1.1315)+
                     (0.0611*Diam_est^1.9208)+(0.1097*Diam_est^1.5530))
      } else if(Health=="D"){
        Reg_B <- ((0.0188*Diam_est^1.3376*0.15^1.5293)+(0.0002*Diam_est^2.4369*0.15^1.1315)+
                     (0.0611*Diam_est^1.9208)+(0.1097*Diam_est^1.5530))*0.95
      }
    } else if (Height_class == "31-130"){
      if(Health=="L"){
        Reg_C1 <- ((0.0188*Diam_est^1.3376*0.80^1.5293)+(0.0002*Diam_est^2.4369*0.80^1.1315)+
                     (0.0611*Diam_est^1.9208)+(0.1097*Diam_est^1.5530))
      } else if(Health=="D"){
        Reg_C1 <- ((0.0188*Diam_est^1.3376*0.80^1.5293)+(0.0002*Diam_est^2.4369*0.80^1.1315)+
                     (0.0611*Diam_est^1.9208)+(0.1097*Diam_est^1.5530))*0.95
      }
    }
  } else if(Species=="Bl"){ 
    if(Height_class == "0-30"){
      if(Health=="L"){
        Reg_B <- ((0.0220*Diam_est^1.6469*0.15^1.1714)+(0.0061*Diam_est^1.8603*0.15^0.7693)+
                   (0.0265*Diam_est^3.6747*0.15^-1.5958)+(0.0509*Diam_est^2.9909*0.15^-1.2271))
      } else if (Health=="D"){
        Reg_B <- ((0.0220*Diam_est^1.6469*0.15^1.1714)+(0.0061*Diam_est^1.8603*0.15^0.7693)+
                     (0.0265*Diam_est^3.6747*0.15^-1.5958)+(0.0509*Diam_est^2.9909*0.15^-1.2271))*0.95
      }
    } else if (Height_class == "31-130"){
      if(Health=="L"){
        Reg_B <- ((0.0220*Diam_est^1.6469*0.80^1.1714)+(0.0061*Diam_est^1.8603*0.80^0.7693)+
                   (0.0265*Diam_est^3.6747*0.80^-1.5958)+(0.0509*Diam_est^2.9909*0.80^-1.2271))
      } else if (Health=="D"){
        Reg_B <- ((0.0220*Diam_est^1.6469*0.80^1.1714)+(0.0061*Diam_est^1.8603*0.80^0.7693)+
                     (0.0265*Diam_est^3.6747*0.80^-1.5958)+(0.0509*Diam_est^2.9909*0.80^-1.2271))*0.95
      }
    }  
  } else if(Species=="Ep"){ 
    if(Height_class == "0-30"){
      if(Health=="L"){
        Reg_B <- ((0.0333*Diam_est^2.0794*0.15^0.6811)+(0.0079*Diam_est^1.9905*0.15^0.6553)+
                     (0.0253*Diam_est^3.1518*0.15^-0.9083)+(0.1361*Diam_est^2.2978*0.15^-1.0934))
      }else if(Health=="D"){
        Reg_B <- ((0.0333*Diam_est^2.0794*0.15^0.6811)+(0.0079*Diam_est^1.9905*0.15^0.6553)+
                     (0.0253*Diam_est^3.1518*0.15^-0.9083)+(0.1361*Diam_est^2.2978*0.15^-1.0934))*0.95
      }
    } else if (Height_class == "31-130"){
      if(Health=="L"){
        Reg_B <- ((0.0333*Diam_est^2.0794*0.80^0.6811)+(0.0079*Diam_est^1.9905*0.80^0.6553)+
                     (0.0253*Diam_est^3.1518*0.80^-0.9083)+(0.1361*Diam_est^2.2978*0.80^-1.0934))
      }else if(Health=="D"){
        Reg_B <- ((0.0333*Diam_est^2.0794*0.80^0.6811)+(0.0079*Diam_est^1.9905*0.80^0.6553)+
                     (0.0253*Diam_est^3.1518*0.80^-0.9083)+(0.1361*Diam_est^2.2978*0.80^-1.0934))*0.95
      }
    }
  } else if(Species=="Hw"){
    if(Height_class == "0-30"){
      if(Health=="L"){
        Reg_B <- ((0.0113*Diam_est^1.9332*0.15^1.1125)+(0.0019*Diam_est^2.3356*0.15^0.6371)+
                     (0.0609*Diam_est^2.0021)+(0.2656*Diam_est^2.0107*0.15^-0.7963))
      } else if(Health=="D"){
        Reg_B <- ((0.0113*Diam_est^1.9332*0.15^1.1125)+(0.0019*Diam_est^2.3356*0.15^0.6371)+
                     (0.0609*Diam_est^2.0021)+(0.2656*Diam_est^2.0107*0.15^-0.7963))*0.95
      }
    } else if (Height_class == "31-130"){
      if(Health=="L"){
        Reg_B <- ((0.0113*Diam_est^1.9332*0.80^1.1125)+(0.0019*Diam_est^2.3356*0.80^0.6371)+
                     (0.0609*Diam_est^2.0021)+(0.2656*Diam_est^2.0107*0.80^-0.7963))
      } else if(Health=="D"){
        Reg_B <- ((0.0113*Diam_est^1.9332*0.80^1.1125)+(0.0019*Diam_est^2.3356*0.80^0.6371)+
                     (0.0609*Diam_est^2.0021)+(0.2656*Diam_est^2.0107*0.80^-0.7963))*0.95
      }
    }
  } else if(Species=="Pl"){
    if(Height_class == "0-30"){
      if(Health=="L"){
        Reg_B <- ((0.0239*Diam_est^1.6827*0.15^1.1878)+(0.0117*Diam_est^1.6398*0.15^0.6524)+
                     (0.0285*Diam_est^3.3764*0.15^-1.4395)+(0.0769*Diam_est^2.6834*0.15^-1.2484))
      } else if(Health=="D"){
        Reg_B <- ((0.0239*Diam_est^1.6827*0.15^1.1878)+(0.0117*Diam_est^1.6398*0.15^0.6524)+
                     (0.0285*Diam_est^3.3764*0.15^-1.4395)+(0.0769*Diam_est^2.6834*0.15^-1.2484))*0.95
      }
    } else if (Height_class == "31-130"){
      if(Health=="L"){
        Reg_B <- ((0.0239*Diam_est^1.6827*0.80^1.1878)+(0.0117*Diam_est^1.6398*0.80^0.6524)+
                     (0.0285*Diam_est^3.3764*0.80^-1.4395)+(0.0769*Diam_est^2.6834*0.80^-1.2484))
      } else if(Health=="D"){
        Reg_B <- ((0.0239*Diam_est^1.6827*0.80^1.1878)+(0.0117*Diam_est^1.6398*0.80^0.6524)+
                     (0.0285*Diam_est^3.3764*0.80^-1.4395)+(0.0769*Diam_est^2.6834*0.80^-1.2484))*0.95
      }
    }
  } else if(Species=="Sx"){
    if(Height_class == "0-30"){
      if(Health=="L"){
        Reg_B <-((0.0133*Diam_est^1.3303*0.15^1.6877)+(0.0086*Diam_est^1.6216*0.15^0.8192)+
                    (0.0428*Diam_est^2.7965*0.15^-0.7328)+(0.0854*Diam_est^2.4388*0.15^-0.7630))
      }else if(Health=="D"){
        Reg_B <-((0.0133*Diam_est^1.3303*0.15^1.6877)+(0.0086*Diam_est^1.6216*0.15^0.8192)+
                    (0.0428*Diam_est^2.7965*0.15^-0.7328)+(0.0854*Diam_est^2.4388*0.15^-0.7630))*0.95
      }
    } else if (Height_class == "31-130"){
      if(Health=="L"){
        Reg_B <- ((0.0133*Diam_est^1.3303*0.80^1.6877)+(0.0086*Diam_est^1.6216*0.80^0.8192)+
                     (0.0428*Diam_est^2.7965*0.80^-0.7328)+(0.0854*Diam_est^2.4388*0.80^-0.7630))  
      }else if(Health=="D"){
        Reg_B <- ((0.0133*Diam_est^1.3303*0.80^1.6877)+(0.0086*Diam_est^1.6216*0.80^0.8192)+
                     (0.0428*Diam_est^2.7965*0.80^-0.7328)+(0.0854*Diam_est^2.4388*0.80^-0.7630))*0.95
      }
    }
  } else if(Species=="Fd"){
    if(Height_class == "0-30"){
      if(Health=="L"){
        Reg_B <- ((0.0191*Diam_est^1.5365*0.15^1.3634)+(0.0083*Diam_est^2.4811)+
                     (0.0351*Diam_est^2.2421)+(0.0718*Diam_est^2.2935*0.15^-0.4744))
      }else if(Health=="D"){
        Reg_B <- ((0.0191*Diam_est^1.5365*0.15^1.3634)+(0.0083*Diam_est^2.4811)+
                     (0.0351*Diam_est^2.2421)+(0.0718*Diam_est^2.2935*0.15^-0.4744))*0.95
      }
    } else if (Height_class == "31-130"){
      if(Health=="L"){
        Reg_B <- ((0.0191*Diam_est^1.5365*0.80^1.3634)+(0.0083*Diam_est^2.4811)+
                     (0.0351*Diam_est^2.2421)+(0.0718*Diam_est^2.2935*0.80^-0.4744))
      } else if(Health=="D"){
        Reg_B <- ((0.0191*Diam_est^1.5365*0.80^1.3634)+(0.0083*Diam_est^2.4811)+
                     (0.0351*Diam_est^2.2421)+(0.0718*Diam_est^2.2935*0.80^-0.4744))*0.95
      }
    }
  } else if(Species=="UC"){ # average of conifers used
    if(Height_class == "0-30"){
      if(Health=="L"){
        Reg_B <- ((0.0276*Diam_est^1.6868*0.15^1.0953)+(0.0101*Diam_est^1.8486*0.15^0.5525)+
                     (0.0313*Diam_est^2.9974*0.15^-1.0383)+(0.1379*Diam_est^2.3981*0.15^-1.0418))
      } else if(Health=="D"){
        Reg_B <- ((0.0276*Diam_est^1.6868*0.15^1.0953)+(0.0101*Diam_est^1.8486*0.15^0.5525)+
                     (0.0313*Diam_est^2.9974*0.15^-1.0383)+(0.1379*Diam_est^2.3981*0.15^-1.0418))*0.95
      }
    } else if (Height_class == "31-130"){
      if(Health=="L"){
        Reg_B <- ((0.0276*Diam_est^1.6868*0.80^1.0953)+(0.0101*Diam_est^1.8486*0.80^0.5525)+
                     (0.0313*Diam_est^2.9974*0.80^-1.0383)+(0.1379*Diam_est^2.3981*0.80^-1.0418))
      } else if(Health=="D"){
        Reg_B <- ((0.0276*Diam_est^1.6868*0.80^1.0953)+(0.0101*Diam_est^1.8486*0.80^0.5525)+
                     (0.0313*Diam_est^2.9974*0.80^-1.0383)+(0.1379*Diam_est^2.3981*0.80^-1.0418))*0.95
      }
    }
  } else if(Species=="Lw"){
    if(Height_class == "0-30"){
      if(Health=="L"){
        Reg_B <- ((0.0276*Diam_est^1.6868*0.15^1.0953)+(0.0101*Diam_est^1.8486*0.15^0.5525)+
                     (0.0313*Diam_est^2.9974*0.15^-1.0383)+(0.1379*Diam_est^2.3981*0.15^-1.0418))
      }else if(Health=="D"){
        Reg_B <- ((0.0276*Diam_est^1.6868*0.15^1.0953)+(0.0101*Diam_est^1.8486*0.15^0.5525)+
                     (0.0313*Diam_est^2.9974*0.15^-1.0383)+(0.1379*Diam_est^2.3981*0.15^-1.0418))*0.95
      }
    } else if (Height_class == "31-130"){
      if(Health=="L"){
        Reg_B <- ((0.0276*Diam_est^1.6868*0.80^1.0953)+(0.0101*Diam_est^1.8486*0.80^0.5525)+
                     (0.0313*Diam_est^2.9974*0.80^-1.0383)+(0.1379*Diam_est^2.3981*0.80^-1.0418))
      } else if(Health=="D"){
        Reg_B <- ((0.0276*Diam_est^1.6868*0.80^1.0953)+(0.0101*Diam_est^1.8486*0.80^0.5525)+
                     (0.0313*Diam_est^2.9974*0.80^-1.0383)+(0.1379*Diam_est^2.3981*0.80^-1.0418))*0.95
      }
    } else {
      print(paste("Species",Species,"not found"))
      Reg_B <- NA
    }
  }
  return(Reg_B)
}


#--------------------- Canopy fuel load----------------------------#
# Cnaopy biomass is calculated using Ung et al. 2008 allometric equations. biomass is in kg. 
# Only using foliage & branches for canopy fuel load

# Standing dead (SD) A decay class and species specificstructural reduction
# factor is applied to each of the components of the allometric equation i.e. bark, bole, top. This accounts for the loss 
# of biomass through decay (Domke et al. 2011). A species and decay class density reduction factor is applied, this 
# accounts for the density loss through decay (Harmon et al. 2011).

# SDCarbon (kg) = BIOMASS x SRF x DCRF
# SDCarbon (kg) = ((Yfoliage*SRF) + (Ybranches*SRF)) X DCRF

TreeCanopyBiomassFN <- function(Species,DBH,HT,Tree_class){
  if(is.na(Species)){
    print(paste("Species is not found"))
    Sp_CB <- NA
  } else if(Species=="At"){
    if(Tree_class < 3 ){
      Sp_CB <-(0.0150*DBH^2.9068*HT^-0.6306)+(0.0284*DBH^1.6020)
    } else if (Tree_class == 3){
      Sp_CB <-((0.0150*DBH^2.9068*HT^-0.6306)*1)*0.97
    } else if (Tree_class == 4){
      Sp_CB <-((0.0150*DBH^2.9068*HT^-0.6306)*0.5)*0.75
    } else if(Tree_class == 5){
      Sp_CB <-((0.0150*DBH^2.9068*HT^-0.6306)*0.2)*0.868
    } else if(Tree_class == 6){
      Sp_CB <-((0.0150*DBH^2.9068*HT^-0.6306)*0.1)*0.613
    } else if(Tree_class == 7){
      Sp_CB <-((0.0150*DBH^2.9068*HT^-0.6306)*0)*0.613
    } else if(Tree_class == 8){
      Sp_CB <-((0.0150*DBH^2.9068*HT^-0.6306)*0)*0.613
    }
    
  } else if(Species=="Ac"){
    if(Tree_class < 3){
      Sp_CB <-(0.0131*DBH^2.5760)+(0.0224*DBH^1.8368)
    } else if (Tree_class == 3){
      Sp_CB <-((0.0131*DBH^2.5760)*1)*1.006
    } else if (Tree_class == 4){
      Sp_CB <-((0.0131*DBH^2.5760)*0.5)*0.793
    } else if (Tree_class == 5){
      Sp_CB <-((0.0131*DBH^2.5760)*0.2)*0.868
    } else if (Tree_class == 6){
      Sp_CB <-((0.0131*DBH^2.5760)*0.1)*0.613
    } else if (Tree_class == 7){
      Sp_CB <-((0.0131*DBH^2.5760)*0)*0.613
    } else if (Tree_class == 8){
      Sp_CB <-((0.0131*DBH^2.5760)*0)*0.613
    }
  } else if(Species=="Cw"){
    if(Tree_class < 3){
      Sp_CB <-(0.0611*DBH^1.9208)+(0.1097*DBH^1.5530)
    } else if (Tree_class == 3){
      Sp_CB <-((0.0131*DBH^2.5760)*1)*1.040
    } else if (Tree_class == 4){
      Sp_CB <-((0.0131*DBH^2.5760)*0.5)*0.960
    } else if (Tree_class == 5){
      Sp_CB <-((0.0131*DBH^2.5760)*0.2)*1.064
    } else if (Tree_class == 6){
      Sp_CB <-((0.0131*DBH^2.5760)*0.1)*0.656
    } else if (Tree_class == 7){
      Sp_CB <-((0.0131*DBH^2.5760)*0)*0.656
    } else if (Tree_class == 8){
      Sp_CB <-((0.0131*DBH^2.5760)*0)*0.656
    }
  } else if(Species=="Bl"){
    if(Tree_class < 3){
      Sp_CB <-(0.0265*DBH^3.6747*HT^-1.5958)+(0.0509*DBH^2.9909*HT^-1.2271)
    } else if (Tree_class == 3){
      Sp_CB <-((0.0265*DBH^3.6747*HT^-1.5958)*1)*1.04
    } else if (Tree_class == 4){
      Sp_CB <-((0.0265*DBH^3.6747*HT^-1.5958)*0.5)*1.068
    } else if (Tree_class == 5){
      Sp_CB <-((0.0265*DBH^3.6747*HT^-1.5958)*0.2)*1
    } else if (Tree_class == 6){
      Sp_CB <-((0.0265*DBH^3.6747*HT^-1.5958)*0.1)*0.696
    } else if (Tree_class == 7){
      Sp_CB <-((0.0265*DBH^3.6747*HT^-1.5958)*0)*0.696
    } else if (Tree_class == 8){
      Sp_CB <-((0.0265*DBH^3.6747*HT^-1.5958)*0)*0.696
    }  
  } else if(Species=="Ep"){
    if(Tree_class < 3){
      Sp_CB <-(0.0253*DBH^3.1518*HT^-0.9083)+(0.1361*DBH^2.2978*HT^-1.0934)
    } else if(Tree_class == 3){
      Sp_CB <-((0.0253*DBH^3.1518*HT^-0.9083)*1)*1.016
    } else if(Tree_class == 4){
      Sp_CB <-((0.0253*DBH^3.1518*HT^-0.9083)*0.5)*0.713
    } else if(Tree_class == 5) {
      Sp_CB <-((0.0253*DBH^3.1518*HT^-0.9083)*0.2)*0.777
    } else if(Tree_class == 6) {
      Sp_CB <-((0.0253*DBH^3.1518*HT^-0.9083)*0.1)*0.439
    } else if(Tree_class == 7) {
      Sp_CB <-((0.0253*DBH^3.1518*HT^-0.9083)*0)*0.439
    } else if(Tree_class == 8) {
      Sp_CB <-((0.0253*DBH^3.1518*HT^-0.9083)*0)*0.439
    }
  } else if(Species=="Hw"){
    if(Tree_class < 3){
      Sp_CB <(0.0609*DBH^2.0021)+(0.2656*DBH^2.0107*HT^-0.7963)
    } else if(Tree_class == 3){
      Sp_CB <-((0.0253*DBH^3.1518*HT^-0.9083)*1)*1.040
    } else if(Tree_class == 4) {
      Sp_CB <-((0.0253*DBH^3.1518*HT^-0.9083)*0.5)*1.080
    } else if(Tree_class == 5) {
      Sp_CB <-((0.0253*DBH^3.1518*HT^-0.9083)*0.2)*0.848
    } else if(Tree_class == 6) {
      Sp_CB <-((0.0253*DBH^3.1518*HT^-0.9083)*0.1)*0.525
    } else if(Tree_class == 7) {
      Sp_CB <-((0.0253*DBH^3.1518*HT^-0.9083)*0)*0.525
    } else if(Tree_class == 8) {
      Sp_CB <-((0.0253*DBH^3.1518*HT^-0.9083)*0)*0.525
    }
  } else if(Species=="Pl"){
    if(Tree_class < 3){
      Sp_CB <-(0.0285*DBH^3.3764*HT^-1.4395)+(0.0769*DBH^2.6834*HT^-1.2484)
    } else if(Tree_class == 3){
      Sp_CB <-((0.0285*DBH^3.3764*HT^-1.4395)*1)*0.98
    } else if(Tree_class == 4){
      Sp_CB <-((0.0285*DBH^3.3764*HT^-1.4395)*0.5)*1.04
    } else if(Tree_class == 5){
      Sp_CB <-((0.0285*DBH^3.3764*HT^-1.4395)*0.2)*1.02
    } else if(Tree_class == 6){
      Sp_CB <-((0.0285*DBH^3.3764*HT^-1.4395)*0.1)*0.727
    } else if(Tree_class == 7){
      Sp_CB <-((0.0285*DBH^3.3764*HT^-1.4395)*0)*0.727
    } else if(Tree_class == 8){
      Sp_CB <-((0.0285*DBH^3.3764*HT^-1.4395)*0)*0.727           
    }
  } else if(Species=="Sx"){
    if(Tree_class < 3){
      Sp_CB <-(0.0428*DBH^2.7965*HT^-0.7328)+(0.0854*DBH^2.4388*HT^-0.7630)
    } else if(Tree_class == 3){  
      Sp_CB <-((0.0428*DBH^2.7965*HT^-0.7328)*1)*0.996
    } else if(Tree_class == 4){
      Sp_CB <-((0.0428*DBH^2.7965*HT^-0.7328)*0.5)*0.943
    } else if(Tree_class == 5){
      Sp_CB <-((0.0428*DBH^2.7965*HT^-0.7328)*0.2)*0.991
    } else if(Tree_class == 6){
      Sp_CB <-((0.0428*DBH^2.7965*HT^-0.7328)*0.1)*0.555
    } else if(Tree_class == 7){
      Sp_CB <-((0.0428*DBH^2.7965*HT^-0.7328)*0)*0.555
    } else if(Tree_class == 8){
      Sp_CB <-((0.0428*DBH^2.7965*HT^-0.7328)*0)*0.555
    }
  } else if(Species=="Fd"){
    if(Tree_class < 3){
      Sp_CB <-(0.0351*DBH^2.2421)+(0.0718*DBH^2.2935*HT^-0.4744)
    } else if(Tree_class == 3){
      Sp_CB <-((0.0351*DBH^2.2421)*1)*0.892
    } else if(Tree_class == 4){
      Sp_CB <-((0.0351*DBH^2.2421)*0.5)*0.831
    } else if(Tree_class == 5){
      Sp_CB <-((0.0351*DBH^2.2421)*0.2)*0.591
    } else if(Tree_class == 6){
      Sp_CB <-((0.0351*DBH^2.2421)*0.1)*0.433
    } else if(Tree_class == 7){
      Sp_CB <-((0.0351*DBH^2.2421)*0)*0.433
    } else if(Tree_class == 8){
      Sp_CB <-((0.0351*DBH^2.2421)*0)*0.433
    }
  } else if(Species=="UC"){
    if(Tree_class<3){
      Sp_CB <-(0.0313*DBH^2.9974*HT^-1.0383)+(0.1379*DBH^2.3981*HT^-1.0418)
    } else if(Tree_class == 3){
      Sp_CB <-((0.0313*DBH^2.9974*HT^-1.0383)*1)*1.005
    } else if(Tree_class == 4){
      Sp_CB <-((0.0313*DBH^2.9974*HT^-1.0383)*0.5)*1.017
    } else if(Tree_class == 5){
      Sp_CB <-((0.0313*DBH^2.9974*HT^-1.0383)*0.2)*1.004
    } else if(Tree_class == 6){
      Sp_CB <-((0.0313*DBH^2.9974*HT^-1.0383)*0.1)*0.659
    } else if(Tree_class == 7){
      Sp_CB <-((0.0313*DBH^2.9974*HT^-1.0383)*0)*0.659
    } else if(Tree_class == 8){
      Sp_CB <-((0.0313*DBH^2.9974*HT^-1.0383)*0)*0.659
    }
  } else if(Species=="Lw"){ #Using Unknown conifer
    if(Tree_class < 3){
      Sp_CB <-(0.0313*DBH^2.9974*HT^-1.0383)+(0.1379*DBH^2.3981*HT^-1.0418)
    } else if(Tree_class == 3){
      Sp_CB <-((0.0313*DBH^2.9974*HT^-1.0383)*1)*1.005
    } else if(Tree_class == 4){
      Sp_CB <-((0.0313*DBH^2.9974*HT^-1.0383)*0.5)*1.017
    } else if(Tree_class == 5){
      Sp_CB <-((0.0313*DBH^2.9974*HT^-1.0383)*0.2)*1.004
    } else if(Tree_class == 6){
      Sp_CB <-((0.0313*DBH^2.9974*HT^-1.0383)*0.1)*0.659
    } else if(Tree_class == 7){
      Sp_CB <-((0.0313*DBH^2.9974*HT^-1.0383)*0)*0.659
    } else if(Tree_class == 8){
      Sp_CB <-((0.0313*DBH^2.9974*HT^-1.0383)*0)*0.659
    }
  } else {
    print(paste("Species",Species,"not found"))
    Sp_CB <- NA
  }
  return(Sp_CB)
}


######################### Woody debris biomass ##########################################

# cwdBIOMASS (Mg/ha) = volume(m3/ha)
#                     x structural reduction factor # decay class specific (Fraver et al. 2013)
#                     x Absolute density(g/cm3) # species and decay class specific (Harmon et al. 2008)


cwdBiomassFN <- function(volume_ha, Decay_class, Species){
  if(is.na(Species)){
    print(paste("Species is not found"))
    DC_Sp_B <- NA
  } else if (Species == "Pl"){
    if(Decay_class == "1"){
      DC_Sp_B <-(volume_ha*1*0.378)
    } else if (Decay_class == "2"){
      DC_Sp_B <-(volume_ha*1*0.367)
    } else if (Decay_class == "3"){
      DC_Sp_B <-(volume_ha*1*0.276)
    } else if (Decay_class == "4"){
      DC_Sp_B <-(volume_ha*0.8*0.169)
    } else if (Decay_class == "5"){
      DC_Sp_B <-(volume_ha*0.412*0.164)
    }
  } else if (Species == "Sx"){
    if (Decay_class == "1"){
      DC_Sp_B <-(volume_ha*1*0.393)
    } else if (Decay_class == "2"){
      DC_Sp_B <-(volume_ha*1*0.285)
    } else if (Decay_class == "3"){
      DC_Sp_B <-(volume_ha*1*0.28)
    } else if (Decay_class == "4"){
      DC_Sp_B <-(volume_ha*0.8*0.136)
    } else if (Decay_class == "5") {
      DC_Sp_B <-(volume_ha*0.412*0.129)
    }
  } else if (Species == "Bl"){
    if (Decay_class == "1"){
      DC_Sp_B <-(volume_ha*1*0.371)
    } else if (Decay_class == "2"){
      DC_Sp_B <-(volume_ha*1*0.288)
    } else if (Decay_class == "3"){
      DC_Sp_B <-(volume_ha*1*0.233)
    } else if (Decay_class == "4"){
      DC_Sp_B <-(volume_ha*0.8*0.152)
    } else if (Decay_class == "5") {
      DC_Sp_B <-(volume_ha*0.412*0.117)
    }
  } else if (Species == "UC"){
    if (Decay_class == "1"){
      DC_Sp_B <-(volume_ha*1*0.381)
    } else if (Decay_class == "2"){
      DC_Sp_B <-(volume_ha*1*0.313)
    } else if (Decay_class == "3"){
      DC_Sp_B <-(volume_ha*0.8*0.152)
    } else if (Decay_class == "4"){
      DC_Sp_B <-(volume_ha*0.8*0.152)
    } else if (Decay_class == "5") {
      DC_Sp_B <-(volume_ha*0.412*0.137)
    }
  } else if (Species == "At"){
    if (Decay_class == "1"){
      DC_Sp_B <-(volume_ha*1*0.353)
    } else if (Decay_class == "2"){
      DC_Sp_B <-(volume_ha*1*0.422)
    } else if (Decay_class == "3"){
      DC_Sp_B <-(volume_ha*1*0.299)
    } else if (Decay_class == "4"){
      DC_Sp_B <-(volume_ha*0.8*0.16)
    } else if (Decay_class == "5") {
      DC_Sp_B <-(volume_ha*0.412*0.11)
    }
  } else if (Species == "Ep"){
    if (Decay_class == "1"){
      DC_Sp_B <-(volume_ha*1*0.469)
    } else if (Decay_class == "2"){
      DC_Sp_B <-(volume_ha*1*0.403)
    } else if (Decay_class == "3"){
      DC_Sp_B <-(volume_ha*1*0.352)
    } else if (Decay_class == "4"){
      DC_Sp_B <-(volume_ha*0.8*0.17)
    } else if (Decay_class == "5") {
      DC_Sp_B <-(volume_ha*0.412*0.11)
    }
  } else if (Species == "UD"){
    if (Decay_class == "1"){
      DC_Sp_B <-(volume_ha*1*0.392)
    } else if (Decay_class == "2"){
      DC_Sp_B <-(volume_ha*1*0.416)
    } else if (Decay_class == "3"){
      DC_Sp_B <-(volume_ha*1*0.317)
    } else if (Decay_class == "4"){
      DC_Sp_B <-(volume_ha*0.8*0.163)
    } else if (Decay_class == "5"){
      DC_Sp_B <-(volume_ha*0.412*0.11)
    }
  } else if (Species == "U"){
    if (Decay_class == "1"){
      DC_Sp_B <-(volume_ha*1*0.386)
    } else if (Decay_class == "2"){
      DC_Sp_B <-(volume_ha*1*0.365)
    } else if (Decay_class == "3"){
      DC_Sp_B <-(volume_ha*1*0.29)
    } else if (Decay_class == "4"){
      DC_Sp_B <-(volume_ha*0.8*0.158)
    } else if (Decay_class == "5"){
      DC_Sp_B <-(volume_ha*0.412*0.123)
    }
  } else {
    DC_Sp_B <- 0
  }
  return(DC_Sp_B)  
}


# fwdBIOMASS (Mg/ha) = volume(m3/ha)
#                     x Live wood density(g/cm3) # use unknown species (Harmon et al. 2008)
#                     x Decay reduction factor for each size class (Harmon and Fasth website)


fwdBiomassFN <- function(Diam_class, volume){
  if(is.na(Diam_class)){
    print(paste("Diam_class is not found"))
    B <- NA
  } else {
    if (Diam_class == "1.1-2.5"){
      B <- (volume*0.41*0.81)
    } else if (Diam_class == "2.6-5"){
      B <- (volume*0.41*1)
    } else if (Diam_class == "5.1-7.5"){
      B <- (volume*0.39*0.99)
    } else {
      print(paste("Diam_class",Species,"not found"))
      B <- NA
    }
  }
  return(B)
}



