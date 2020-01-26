<template>
    <div style="text-align: center;">


          <h1 class="font-weight-light light-blue--text" style="text-align:left">Bekommen heute weniger Studierende BAföG?</h1>

            <v-container
                id="scroll-target"
                style="padding: 0"
            >
            <br>


            <div id="dev_graph">
                <h3 style="text-align:left; width: 93%; position: relative; left: 7%" v-scroll="onScrollVerlauf">
                    Deutschlandweite Entwicklung des Anteils der BAföG-Empfänger*innen unter den Studierenden
                </h3>
                <div id="dev-y-axis">
                    Anteil der <br>BAföG-Empfänger*innen
                </div>
                <div>
                    <div id="perc30">
                        <div class="perc">30%</div>
                        <hr class="descr">
                    </div>
                    <div id="perc25">
                        <div class="perc">25%</div>
                        <hr class="descr">
                    </div>
                    <div id="perc20">
                        <div class="perc">20%</div>
                        <hr class="descr">
                    </div>
                </div>

                    <v-sparkline
                       id = "spark_dev"
                       v-if="verlauf"
                          :value="percOverYears.value"
                          :gradient="percOverYears.gradient"
                          :smooth="percOverYears.radius || false"
                          :padding="percOverYears.padding"
                          :line-width="percOverYears.width"
                          :stroke-linecap="percOverYears.lineCap"
                          :gradient-direction="percOverYears.gradientDirection"
                          :fill="percOverYears.fill"
                          :type="percOverYears.type"
                          :auto-line-width="percOverYears.autoLineWidth"
                          auto-draw
                          auto-draw-duration=3000
                          height=70
                   ></v-sparkline>
                   <div id="slider-dev">
                   <v-slider
                       v-model="slider.value"
                       step="5"
                       ticks="always"
                       tick-size="5"
                       :tick-labels="slider.ticksLabels"
                   ></v-slider>
               </div>
           </div>
           <div style="font-weight: bold;  font-size: 15px;">Jahr</div>



          <v-row align="start" justify="center">
            <v-flex xs12 sm9 md5>
              <v-img
                v-if="slider.value==0"
                src="../assets/1998.png"
                contain
                style="width: 100%;"
              ></v-img>
              <v-img
                v-if="slider.value==5"
                src="../assets/1999.png"
                contain
                style="height: 100%;"
              ></v-img>
              <v-img
                v-if="slider.value==10"
                src="../assets/2000.png"
                contain
                style="height: 100%;"
              ></v-img>
              <v-img
                v-if="slider.value==15"
                src="../assets/2001.png"
                contain
                style="height: 100%;"
              ></v-img>
              <v-img
                v-if="slider.value==20"
                src="../assets/2002.png"
                contain
                style="height: 100%;"
              ></v-img>
              <v-img
                v-if="slider.value==25"
                src="../assets/2003.png"
                contain
                style="height: 100%;"
              ></v-img>
              <v-img
                v-if="slider.value==30"
                src="../assets/2004.png"
                contain
                style="height: 100%;"
              ></v-img>
              <v-img
                v-if="slider.value==35"
                src="../assets/2005.png"
                contain
                style="height: 100%;"
              ></v-img>
              <v-img
                v-if="slider.value==40"
                src="../assets/2006.png"
                contain
                style="height: 100%;"
              ></v-img>
              <v-img
                v-if="slider.value==45"
                src="../assets/2007.png"
                contain
                style="height: 100%;"
              ></v-img>
              <v-img
                v-if="slider.value==50"
                src="../assets/2008.png"
                contain
                style="height: 100%;"
              ></v-img>
              <v-img
                v-if="slider.value==55"
                src="../assets/2009.png"
                contain
                style="height: 100%;"
              ></v-img>
              <v-img
                v-if="slider.value==60"
                src="../assets/2010.png"
                contain
                style="height: 100%;"
              ></v-img>
              <v-img
                v-if="slider.value==65"
                src="../assets/2011.png"
                contain
                style="height: 100%;"
              ></v-img>
              <v-img
                v-if="slider.value==70"
                src="../assets/2012.png"
                contain
                style="height: 100%;"
              ></v-img>
              <v-img
                v-if="slider.value==75"
                src="../assets/2013.png"
                contain
                style="height: 100%;"
              ></v-img>
              <v-img
                v-if="slider.value==80"
                src="../assets/2014.png"
                contain
                style="height: 100%;"
              ></v-img>
              <v-img
                v-if="slider.value==85"
                src="../assets/2015.png"
                contain
                style="height: 100%;"
              ></v-img>
              <v-img
                v-if="slider.value==90"
                src="../assets/2016.png"
                contain
                style="height: 100%;"
              ></v-img>
              <v-img
                v-if="slider.value==95"
                src="../assets/2017.png"
                contain
                style="height: 100%;"
              ></v-img>
              <v-img
                v-if="slider.value==100"
                src="../assets/2018.png"
                contain
                style="height: 100%;"
              ></v-img>
            </v-flex>
            <v-flex xs12 sm3 md2 class="legend pt-5">
                <div>
                    <div class="legend-color" style="background-color: #E1F5FE;"></div>
                    größer 15% bis 20%
                </div>
                <div>
                    <div class="legend-color" style="background-color: #81D4FA;"></div>
                    größer 20% bis 25%
                </div>
                <div>
                    <div class="legend-color" style="background-color: #29B6F6;"></div>
                    größer 25% bis 30%
                </div>
                <div>
                    <div class="legend-color" style="background-color: #0288D1;"></div>
                    größer 30% bis 35%
                </div>
                <div>
                    <div class="legend-color" style="background-color: #0266b5;"></div>
                    größer 35% bis 40%
                </div>
                <div>
                    <div class="legend-color" style="background-color: #014880;"></div>
                    größer 40% bis 45%
                </div>
                <div>
                    <div class="legend-color" style="background-color: #00335c;"></div>
                    größer 45% bis 50%
                </div>
            </v-flex>
            <v-flex xs12 md5 style="text-align: left" class="align-self-stretch">
               <div class="pa-5" v-if="slider.value == 0">
                   <h2>1998</h2>
                   <p class="year-texts">
                       In den 90er Jah&shy;ren sinkt der An&shy;teil der BAföG-Empfän&shy;ger*innen ver&shy;mut&shy;lich auf&shy;grund der un&shy;zu&shy;rei&shy;chenden An&shy;passung der Frei&shy;beträge und Bedarfs&shy;sätze
                       stark ab, so&shy;dass 1998 ledig&shy;lich 20,6 % der Studie&shy;renden eine För&shy;derung durch BAföG er&shy;hal&shy;ten.
                </p>
               </div>
               <div class="pa-5" v-if="slider.value == 5">
                   <h2>1999</h2>
                   <p class="year-texts">
                       Im Jahr 1999 kün&shy;digt die neue rot-grüne Bun&shy;des&shy;re&shy;gierung eine grund&shy;le&shy;gende Re&shy;form an.
                       Das durch das Deu&shy;tsche Stu&shy;die&shy;ren&shy;den&shy;werk prä&shy;fe&shy;rierte "Drei-Körbe-Mo&shy;dell" setzt sich in der Bun&shy;des&shy;re&shy;gie&shy;rung un&shy;ter Schrö&shy;der je&shy;doch nicht durch.
                       Es werden letzt&shy;end&shy;lich die Frei&shy;be&shy;träge er&shy;höht, so&shy;dass die Re&shy;form ins&shy;gesamt weit hin&shy;ter den selbst ge&shy;steck&shy;ten Er&shy;war&shy;tungen zurück&shy;bleibt.</p>
               </div>
               <div class="pa-5" v-if="slider.value == 10">
                   <h2>2000</h2>
                   <p class="year-texts">
                       Zur Jahr&shy;tau&shy;send&shy;wen&shy;de än&shy;dert sich der An&shy;teil der ge&shy;för&shy;derten Stud&shy;ie&shy;renden kaum.
                   </p>
               </div>
               <div class="pa-5" v-if="slider.value == 15">
                   <h2>2001</h2>
                   <p class="year-texts">
                       Im Jahr 2001 wird die von der rot-grü&shy;nen Bun&shy;des&shy;re&shy;gie&shy;rung an&shy;ge&shy;kün&shy;digte Reform be&shy;schlos&shy;sen.
                       Hier&shy;durch steigt auch wie&shy;der der An&shy;teil der ge&shy;för&shy;derten Stud&shy;ie&shy;ren&shy;den.
                       Unter an&shy;derem wird das Kin&shy;der&shy;geld nun nicht mehr auf das Ein&shy;kom&shy;men der El&shy;tern an&shy;ge&shy;rechnet.
                       Auch die Gesamt&shy;dar&shy;lehens&shy;schuld der Stu&shy;die&shy;renden wird auf ma&shy;ximal 20.000 Mark be&shy;schränkt.
                   </p>
               </div>
               <div class="pa-5" v-if="slider.value == 20">
                   <h2>2002</h2>
                   <p class="year-texts">Im Jahr 2002 stei&shy;gen die BAföG-Ge&shy;samt&shy;aus&shy;ga&shy;ben von 1,1 auf 1,3 Millionen Euro. Die Zahl der ge&shy;för&shy;derten Stu&shy;die&shy;ren&shy;den nimmt eben&shy;so zu.</p>
               </div>
               <div class="pa-5" v-if="slider.value == 25">
                   <h2>2003</h2>
                   <p class="year-texts">Laut dem 15. BAföG-Be&shy;richt der Bun&shy;des&shy;re&shy;gie&shy;rung vom März 2003 er&shy;hal&shy;ten mehr Stu&shy;die&shy;rende BAföG als vor der Re&shy;form im Ok&shy;to&shy;ber 2002.</p>
               </div>
               <div class="pa-5" v-if="slider.value == 30">
                   <h2>2004</h2>
                   <p class="year-texts">Im Jahr 2004 legt die Zahl der BAföG-Em&shy;pfän&shy;ger*innen bei Stu&shy;die&shy;renden leicht zu. Klei&shy;ne Än&shy;de&shy;rungen wer&shy;den an De&shy;tails vor&shy;ge&shy;nom&shy;men, diese er&shy;hö&shy;hen die För&shy;der&shy;be&shy;trä&shy;ge der be&shy;reits Be&shy;rech&shy;tig&shy;ten je&shy;doch nicht.</p>
               </div>
               <div class="pa-5" v-if="slider.value == 35">
                   <h2>2005</h2>
                   <p class="year-texts">Bis 2005 steigt der An&shy;teil der BAföG-Empfänger*innen dank der BAföG-Re&shy;form von 2001 an.</p>
               </div>
               <div class="pa-5" v-if="slider.value == 40">
                   <h2>2006</h2>
                   <p class="year-texts">
                       In den ers&shy;ten Bun&shy;des&shy;län&shy;dern wer&shy;den Stu&shy;dien&shy;ge&shy;büh&shy;ren ein&shy;ge&shy;führt.
                       Hier&shy;zu ge&shy;hö&shy;ren Nie&shy;der&shy;sach&shy;sen und Nord&shy;rhein-West&shy;falen.
                       In Bay&shy;ern, Ham&shy;burg, Hes&shy;sen und Saar&shy;land wer&shy;den die Stu&shy;dien&shy;ge&shy;büh&shy;ren nur be&shy;schlos&shy;sen.
                       Von vie&shy;len Stu&shy;dieren&shy;den wer&shy;den zu&shy;sätz&shy;liche Studien&shy;kre&shy;dite ge&shy;braucht.
                       Neue De&shy;batten über die Studien&shy;fi&shy;nan&shy;zierung fin&shy;den statt.
                   </p>
               </div>
               <div class="pa-5" v-if="slider.value == 45">
                   <h2>2007</h2>
                   <p class="year-texts">
                       In die&shy;sem Jahr füh&shy;ren wei&shy;tere Bun&shy;des&shy;län&shy;er all&shy;emeine Studien&shy;ge&shy;bühren ein.
                       Für das Som&shy;mer&shy;se&shy;mester wer&shy;den die Ge&shy;büh&shy;ren in den Bun&shy;des&shy;län&shy;der Ba&shy;den-Württem&shy;berg, Bayern und Ham&shy;burg ein&shy;ge&shy;führt.
                       Für das Winter&shy;se&shy;mester 2007/2008 ent&shy;schei&shy;den sich Saar&shy;land und Hessen auch für die Ein&shy;füh&shy;rung der Studien&shy;ge&shy;bühren.
                  </p>
               </div>
               <div class="pa-5" v-if="slider.value == 50">
                   <h2>2008</h2>
                   <p class="year-texts">
                       Das 22. BAföG-Än&shy;de&shy;rungs&shy;gesetz tritt in Kraft.
                       Durch die Än&shy;de&shy;run&shy;gen wer&shy;den Frei&shy;be&shy;trä&shy;ge um 8% er&shy;höht und 400-Euro-Jobs nicht mehr auf den BAföG-För&shy;der&shy;betrag an&shy;ge&shy;rech&shy;net.
                       Ab dem ers&shy;ten Se&shy;mes&shy;ter ist jetzt ein Aus&shy;lands&shy;stu&shy;dium inner&shy;halb Euro&shy;pas und der Schweiz mög&shy;lich.
                       Zum Win&shy;ter&shy;se&shy;mes&shy;ter wer&shy;den die Stu&shy;dien&shy;ge&shy;büh&shy;ren zum ers&shy;ten Mal in Hes&shy;sen ab&shy;ge&shy;schafft.
                </p>
               </div>
               <div class="pa-5" v-if="slider.value == 55">
                   <h2>2009</h2>
                   <p class="year-texts">2009 gibt es keine be&shy;deu&shy;ten&shy;den Neuer&shy;ungen.</p>
               </div>
               <div class="pa-5" v-if="slider.value == 60">
                   <h2>2010</h2>
                   <p class="year-texts">
                       Zum Som&shy;mer&shy;se&shy;mester 2010 schafft das Saar&shy;land (CDU/FDP/Grüne) allge&shy;meine Studien&shy;ge&shy;büh&shy;ren wie&shy;der ab.
                       Durch das 23. BAföG-Än&shy;derungs&shy;ge&shy;setz kann man nun durch BAföG ge&shy;för&shy;dert werden, wenn man vor Voll&shy;endung des 35. Lebens&shy;jahrs einen Master be&shy;ginnt.
                   </p>
               </div>
               <div class="pa-5" v-if="slider.value == 65">
                   <h2>2011</h2>
                   <p class="year-texts">
                       Das Deutsch&shy;land&shy;sti&shy;pen&shy;dium für be&shy;gabte Stu&shy;die&shy;ren&shy;de wird ein&shy;ge&shy;führt.
                       Ein&shy;kommens&shy;un&shy;abhängig be&shy;trägt die För&shy;derung 300 Euro im Monat.
                       Zum Win&shy;ter&shy;se&shy;mester 2011/2012 schafft Nord&shy;rhein-West&shy;falen (SPD/Grüne) all&shy;ge&shy;mei&shy;ne Studien&shy;ge&shy;bühren ab.
                       Die Re&shy;gierung ist eine durch die Lin&shy;ken unter&shy;stützte Minder&shy;heits&shy;re&shy;gierung.
                   </p>
               </div>
               <div class="pa-5" v-if="slider.value == 70">
                   <h2>2012</h2>
                   <p class="year-texts">
                       Die Zahl der geför&shy;derten Stu&shy;dieren&shy;den ist zwar gegen&shy;über dem Vor&shy;jahr um 4,3% ge&shy;stie&shy;gen, aller&shy;dings ist die Gesamt&shy;zahl der Stu&shy;dieren&shy;den noch stär&shy;ker ge&shy;wach&shy;sen, so&shy;dass die Ge&shy;förderten&shy;quote ins&shy;ge&shy;samt ge&shy;sun&shy;ken ist.
                       Das BAföG ist seit 2010 nicht er&shy;höht wor&shy;den, das Ein&shy;kommen der El&shy;tern hin&shy;ge&shy;gen ist ge&shy;stie&shy;gen.
                       Das führt dazu, dass im&shy;mer mehr Stu&shy;die&shy;rende für eine BAföG-För&shy;de&shy;rung zu rei&shy;che El&shy;tern ha&shy;ben.
                       Der freie Zusam&shy;men&shy;schluss von Student*innen&shy;schaf&shy;ten und das Deutsche Stu&shy;den&shy;ten&shy;werk for&shy;dern eine Er&shy;höhung des BAföG-Satzes.
                       Zum Sommer&shy;se&shy;mester schafft Ba&shy;den-Württem&shy;berg (Grüne/SPD) all&shy;ge&shy;meine Stu&shy;dien&shy;ge&shy;büh&shy;ren ab, im da&shy;rauf&shy;fol&shy;genden Win&shy;ter&shy;se&shy;mester zie&shy;hen Ham&shy;burg (SPD) und Bayern (CDU/FDP) nach.
                       In Bayern wird das Ge&shy;setz be&shy;schlossen, nach&shy;dem ein durch die Freien Wäh&shy;ler ini&shy;tiiertes Volks&shy;be&shy;geh&shy;ren zur Ab&shy;schaffung Er&shy;folg hatte.
                   </p>
               </div>
               <div class="pa-5" v-if="slider.value == 75">
                   <h2>2013</h2>
                   <p class="year-texts">
                       Ob&shy;wohl die För&shy;der&shy;si&shy;tu&shy;ation sich zum Vor&shy;jahr nicht stark ver&shy;ändert hat, konn&shy;ten sich Bund und Län&shy;der nicht auf eine Ver&shy;ände&shy;rung des BAföGs ei&shy;nigen.
                   </p>
               </div>
               <div class="pa-5" v-if="slider.value == 80">
                   <h2>2014</h2>
                   <p class="year-texts">
                       Zum Win&shy;ter&shy;se&shy;mes&shy;ter 2014/2015 schafft Nie&shy;der&shy;sach&shy;sen (SPD/Grüne) all&shy;ge&shy;meine Stu&shy;dien&shy;ge&shy;büh&shy;ren als letz&shy;tes Bun&shy;des&shy;land ab.
                   </p>
                </div>
               <div class="pa-5" v-if="slider.value == 85">
                   <h2>2015</h2>
                   <p class="year-texts">
                       Im Rah&shy;men des 25. BAföG-Än&shy;derungs&shy;ge&shy;setz wird die BAföG-Fi&shy;nan&shy;zierung voll&shy;stän&shy;dig durch den Bund über&shy;nom&shy;men.
                   </p>
               </div>
               <div class="pa-5" v-if="slider.value == 90">
                   <h2>2016</h2>
                   <p class="year-texts">
                       Die Ver&shy;än&shy;derun&shy;gen des 25. BAföG-Än&shy;de&shy;rungs&shy;gesetz grei&shy;fen.
                       Die Ein&shy;kommens&shy;frei&shy;be&shy;trä&shy;ge der El&shy;tern wer&shy;den er&shy;höht und ei&shy;gen&shy;es Ver&shy;mö&shy;gen bis 7.500 Euro wird nicht mehr be&shy;rück&shy;sich&shy;tigt.
                       Das von der Bun&shy;des&shy;re&shy;gie&shy;rung de&shy;fi&shy;nier&shy;te Ziel, 110.000 Ge&shy;för&shy;derte zu&shy;sätzlich zu ge&shy;ne&shy;rie&shy;ren, wird je&shy;doch ver&shy;fehlt.
                   </p>
               </div>
               <div class="pa-5" v-if="slider.value == 95">
                   <h2>2017</h2>
                   <p class="year-texts">
                       Der An&shy;teil der Stu&shy;dieren&shy;den, die durch BAföG ge&shy;för&shy;dert wer&shy;den, sinkt kon&shy;tinu&shy;ier&shy;lich.
                       Bei der Bun&shy;des&shy;tags&shy;wahl im Herbst 2017 wer&shy;den mög&shy;liche BAföG-Re&shy;for&shy;men je&shy;doch nicht the&shy;mati&shy;siert.
                   </p>
               </div>
               <div class="pa-5" v-if="slider.value == 100">
                   <h2>2018</h2>
                   <p class="year-texts">
                       Im Jahr 2018 sinkt der An&shy;teil der ge&shy;för&shy;der&shy;ten Stu&shy;dieren&shy;den weiter ab.
                       Es bleibt ab&shy;zu&shy;war&shy;ten, ob sich die&shy;ser Ne&shy;ga&shy;tiv&shy;trend durch die Re&shy;form von 2019 um&shy;kehrt.
                   </p>
               </div>
            </v-flex>
          </v-row>
          </v-container>


          <v-bottom-sheet v-model="sheet" >
            <template v-slot:activator="{ on }">
              <v-btn
                v-on="on"
              >
                <i class="fas fa-plus" style="margin-right: 10px;"></i>Methodik
              </v-btn>
            </template>

            <v-sheet class="pa-10">
                <i class="fas fa-times fa-lg float-right" @click="sheet = !sheet"></i>
              <v-row >
                  <v-flex xs12 md4 class="pa-3">
                      <div>
                          <i class="fas fa-list-ol icon"></i>
                          Beschreibung der Datenanalyse
                      </div>
                      <br>
                      <div style="font-size:smaller">
                          <h4> Wie wird der Anteil der BAföG-Empfänger*innen berechnet?</h4>
                          <p>
                              Um den An&shy;teil der Stu&shy;dieren&shy;den zu be&shy;rech&shy;nen, die durch BAföG ge&shy;för&shy;dert wer&shy;den, wer&shy;den zwei Da&shy;ten&shy;sätze des sta&shy;tis&shy;tischen Bun&shy;des&shy;amts kom&shy;bi&shy;niert:
                              "Studierende: Bundesländer, Semester, Nationalität, Geschlecht" (Studierendendatensatz) und "BAföG - Geförderte Personen und finanzieller Aufwand: Bundesländer, Jahre, Personenkreis" (BAföG-Datensatz).
                              Da&shy;bei ent&shy;hält der BAföG-Daten&shy;satz die ab&shy;solu&shy;te Zahl der in einem Ka&shy;len&shy;der&shy;jahr ge&shy;för&shy;derten Stu&shy;dierenden.
                              Der Stu&shy;die&shy;renden&shy;daten&shy;satz lie&shy;fert für das Winter&shy;se&shy;mester des sel&shy;ben Jah&shy;res die An&shy;zahl der Stu&shy;dieren&shy;den mit deutscher Staats&shy;bür&shy;ger&shy;schaft.
                              Durch die Di&shy;vi&shy;sion bei&shy;der Wer&shy;te er&shy;gibt sich der An&shy;teil der BAföG-Empfänger*innen auf Bundes- bzw. Länder&shy;ebene für das je&shy;wei&shy;lige Jahr.
                          </p>

                          <h4> Weshalb werden Prozente und keine absoluten Zahlen angegeben?</h4>
                          <p>
                              In der me&shy;dia&shy;len Be&shy;richt&shy;er&shy;stattung zum BAföG fin&shy;den sich häufig Aus&shy;sagen zur ab&shy;soluten Zahl der BAföG-Empfänger*innen.
                              Bei der The&shy;matik sind sol&shy;che An&shy;gaben je&shy;doch nur be&shy;schränkt aus&shy;sage&shy;kräf&shy;tig, da den meis&shy;ten Le&shy;senden nicht bekannt sein dürfte, wie vie&shy;le Stu&shy;dierende es in Deutsch&shy;land gibt.
                              Be&shy;dingt durch Ver&shy;än&shy;derungen im Bildungs&shy;system sind weiter&shy;hin die Stu&shy;dierenden&shy;zah&shy;len im Zeit&shy;raum von 1998 bis 2018 von 1,6 Millionen deutscher Studierende auf 2,5 Millionen an&shy;ge&shy;stie&shy;gen (eigene Be&shy;rech&shy;nung: Stu&shy;dierenden&shy;daten&shy;satz).
                              Durch Prozent&shy;an&shy;gaben kann somit gegen&shy;über abso&shy;luten Zah&shy;len eine Vergleichs&shy;grund&shy;lage ge&shy;schaf&shy;fen wer&shy;den.
                          </p>

                          <h4>Wieso wird nur die Anzahl der deutschen Studierenden betrachtet?</h4>
                          <p>
                              In den ver&shy;gan&shy;ge&shy;nen Jah&shy;ren ist auch die Zahl der aus&shy;län&shy;dischen Stu&shy;dieren&shy;den stark an&shy;ge&shy;stie&shy;gen. Mit&shy;glieder dieses Personen&shy;kreises kön&shy;nen je&shy;doch nur in Aus&shy;nahme&shy;fällen BAföG be&shy;an&shy;tra&shy;gen.
                              Um die Zahl der Stu&shy;die&shy;ren&shy;den, die theo&shy;re&shy;tisch BAföG be&shy;an&shy;tra&shy;gen kön&shy;nen, möglichst gut ab&shy;zuschätzen, wur&shy;den daher nur Stu&shy;die&shy;rende mit deutscher Staats&shy;bürger&shy;schaft in die Be&shy;rech&shy;nung ein&shy;be&shy;zogen.
                              Durch diese Maß&shy;nah&shy;me wird der An&shy;teil der BAföG-Empfänger*innen ten&shy;denziell über&shy;schätzt.
                          </p>

                          <h4>Welche alternativen Berechnungsmethoden gibt es?</h4>
                          <div style="text-align: center; margin-top: 6px;">
                              <img style="width: 100%" src="../assets/berechnung.jpg">
                              <img style="width: 50%" src="../assets/berechnung_leg.jpg">
                          </div>
                          <p>
                              Neben der von uns verwendeten Berechnung (obere Linie, hellblau), gibt es weitere alternative Methoden zur Bestimmung der Gefördertenquoten (Daten aus Iost, 2019).
                              Beispielsweise lässt sich die Gefördertenquote unter allen Studierenden betrachten (untere Linie, dunkelblau).
                              Aufgrund der hier enthaltenen ausländischen Studierenden ist die errechnete Gefördertenquote unter allen Studierenden wesentlich niedriger als in unseren Berechnungen.
                              Im Rahmen der Berichte der Bundesregierung nach §35 BAföG wird weiterhin eine Gefördertenquote unter allen Anspruchsberechtigten bestimmt (mittlere Linie, mittelblau).
                              Diese soll theoretisch den Anteil der geförderten Studierenden möglichst gut abbilden.
                              Allerdings schwanken die Definitionskriterien für Anspruchsberechtigte zwischen den Berichten.
                              Insgesamt wird jedoch ersichtlich, dass wir duch unsere Berechnungsmethodik die BAföG-Quote systematisch überschätzen.
                              Qualitativ zeigt sich für alle drei Berechnungsmethoden eine ähnliche Verlaufsform für die Entwicklung des Anteils der BAföG-Empfänger*innen.
                          </p>
                      </div>
                  </v-flex>
                 <v-flex xs12 md4 class="pa-3">
                     <div>
                         <div>
                             <i class="fas fa-file-alt icon"></i>
                             Textquellen
                         </div>
                         <br>
                         <p style="font-size:smaller">
                             <ul>
                                    <li>
                                      Der Tagesspiegel (2001).
                                      <span style="font-style: italic;">
                                          Das Jahr 2001: Gleiches Bafög für Ost und West.
                                      </span>
                                      Abgerufen unter:
                                      <a href="https://www.tagesspiegel.de/kultur/das-jahr-2001-gleiches-bafoeg-fuer-ost-und-west/190378.html">
                                          https://www.tagesspiegel.de/&#8203;kultur/&#8203;das-jahr-2001-gleiches-bafoeg-fuer-ost-und-west/190378.html
                                      </a>.
                                   </li>

                              <li>
                                Deutsches Studentenwerk (2012).
                                <span style="font-style: italic;">
                                    Das BAföG 2012 erhöhen!
                                </span>
                                Abgerufen unter:
                                <a href="https://www.studentenwerke.de/de/content/das-baf%C3%B6g-2012-erh%C3%B6hen">
                                    https://www.studentenwerke.de/&#8203;de/&#8203;content/&#8203;das-baf%C3%B6g-2012-erh%C3%B6hen
                                </a>.
                             </li>

                             <li>
                                Deutsches Studentenwerk (2019).
                                <span style="font-style: italic;">
                                    Geschichte und Statistik zum BAföG.
                                </span>
                                Abgerufen unter:
                                <a href="https://www.studentenwerke.de/de/content/geschichte-und-statistik-zum-baf%C3%B6g">
                                    https://www.studentenwerke.de/&#8203;de/&#8203;content/&#8203;geschichte-und-statistik-zum-baf%C3%B6g
                                </a>.
                            </li>

                             <li>
                               Iost, Oliver (2013).
                               <span style="font-style: italic;">
                                   Jahresausblick 2013. Was sich für Studierende in Gelddingen ändert.
                               </span>
                               Abgerufen unter:
                               <a href="https://www.bafoeg-rechner.de/Hintergrund/art-1472-jahresausblick-2013.php">
                                   https://www.bafoeg-rechner.de/&#8203;Hintergrund/&#8203;art-1472-jahresausblick-2013.php
                               </a>.
                            </li>

                             <li>
                               Iost, Oliver (2019).
                               <span style="font-style: italic;">
                                   Hintergrund: Die Geschichte des BAföG von 1972 bis heute.
                               </span>
                               Abgerufen unter:
                               <a href="https://www.bafoeg-rechner.de/Hintergrund/geschichte.php">
                                   https://www.bafoeg-rechner.de/&#8203;Hintergrund/&#8203;geschichte.php
                               </a>.
                            </li>

                            <li>
                              Kauder, Björn und Potrafke, Niklas (2013).
                              <span style="font-style: italic;">
                                  Government Ideology and Tuition Fee Policy: Evidence from the German States.
                              </span>
                              CESifo Working Paper, No. 4205, Center for Economic Studies and Ifo Institute (CESifo), Munich.
                           </li>

                            <li>
                              Langer, Markus; Müller, Ulrich und Rölle, Daniel (2006).
                              <span style="font-style: italic;">
                                  CHE-Studienkredit-Test Mai 2006. Der aktuelle Vergleich von Studienkrediten des Centrums für Hochschulentwicklung.
                              </span>
                              Abgerufen unter:
                              <a href="https://www.che.de/download/studienkredit_test_mai_2006_ap76-pdf">
                                  https://www.che.de/&#8203;download/&#8203;studienkredit_test_mai_2006_ap76-pdf
                              </a>.
                           </li>

                            <li>
                              Müller, Ulrich (2013).
                              <span style="font-style: italic;">
                                  CHE-Studienkredit-Test 2013. 33 Studienkredite und Bildungsfonds im Vergleich,
                              </span>
                              Abgerufen unter:
                              <a href="https://www.che.de/wp-content/uploads/upload/AP165_CHE_Studienkredit_Test_2013.pdf">
                                  https://www.che.de/&#8203;wp-content/&#8203;uploads/&#8203;upload/&#8203;AP165_CHE_Studienkredit_Test_2013.pdf
                              </a>.
                           </li>

                           <li>
                             Spiegel Panorama (2008).
                             <span style="font-style: italic;">
                                 Tschüss, Campusmaut. Hessen schafft Studiengebühren ab.
                             </span>
                             Abgerufen unter:
                             <a href="https://www.spiegel.de/lebenundlernen/uni/tschuess-campusmaut-hessen-schafft-studiengebuehren-ab-a-557537.html">
                                 https://www.spiegel.de/&#8203;lebenundlernen/&#8203;uni/&#8203;tschuess-campusmaut-hessen-schafft-studiengebuehren-ab-a-557537.html
                             </a>.
                          </li>

                            <li>
                              Spiegel Panorama (2013).
                              <span style="font-style: italic;">
                                  Niedersachsen. Letztes Bundesland schafft Studiengebühren ab.
                              </span>
                              Abgerufen unter:
                              <a href="https://www.spiegel.de/lebenundlernen/uni/niedersachsen-schafft-die-studiengebuehren-zum-wintersemester-ab-a-938280.html">
                                  https://www.spiegel.de/&#8203;lebenundlernen/&#8203;uni/&#8203;niedersachsen-schafft-die-studiengebuehren-zum-wintersemester-ab-a-938280.html
                              </a>.
                           </li>

                           <li>
                             Wolf, Rainer (2008).
                             <span style="font-style: italic;">
                                 Förderung von Aus- und Fortbildung in Baden-Württemberg – Leistungen nach BAföG und AFBG.
                             </span>
                             Abgerufen unter:
                             <a href="https://www.statistik-bw.de/Service/Veroeff/Monatshefte/20081205">
                                 https://www.statistik-bw.de/&#8203;Service/&#8203;Veroeff/&#8203;Monatshefte/&#8203;20081205
                             </a>.
                          </li>

                             <li>
                               Zeit online (2013).
                               <span style="font-style: italic;">
                                   Landtag beschließt Ende der Studiengebühren in Bayern.
                               </span>
                               Abgerufen unter:
                               <a href="https://www.zeit.de/studium/hochschule/2013-04/studiengebuehren-bayern-abschaffung">
                                   https://www.zeit.de/&#8203;studium/&#8203;hochschule/&#8203;2013-04/&#8203;studiengebuehren-bayern-abschaffung
                               </a>.
                            </li>
                            <li>
                              Deutsches Studentenwerk (2017).
                              <span style="font-style: italic;">
                                  BAföG regelmäßig erhöhen, mehr fürs BAföG werben.
                              </span>
                              Abgerufen unter:
                              <a href="https://www.studentenwerke.de/de/content/baf%C3%B6g-regelm%C3%A4%C3%9Fig-erh%C3%B6hen-mehr-f%C3%BCrs-baf%C3%B6g">
                                  https://www.studentenwerke.de/&#8203;de/&#8203;content/&#8203;baf%C3%B6g-regelm%C3%A4%C3%9Fig-erh%C3%B6hen-mehr-f%C3%BCrs-baf%C3%B6g
                              </a>.
                           </li>



                            </ul>
                         </p>
                     </div>
                     <br>
                     <br>
                     <div>
                         <div>
                             <i class="fab fa-github icon"></i>
                                 Code
                         </div>
                         <br>
                         <div style="font-size:smaller">
                             <ul>
                                 <li>
                                     <a href="https://github.com/clarissastaudt/Datenjournalismus_BAfoeG/blob/master/R/development.R">
                                         https://github.com/&#8203;clarissastaudt/&#8203;Datenjournalismus_BAfoeG/&#8203;blob/&#8203;master/&#8203;R/&#8203;development.R
                                     </a>
                                 </li>
                             </ul>
                         </div>
                     </div>
                 </v-flex>
                 <v-flex xs12 md4 class="pa-3">
                     <div>
                          <div>
                              <i class="fas fa-database icon"></i>
                              Daten
                          </div>
                          <br>
                          <div style="font-size:smaller">
                              <ul>
                                  <li>
                                    Statistisches Bundesamt (2019).
                                    <span style="font-style: italic;">
                                        Studierende: Bundesländer, Semester, Nationalität, Geschlecht (21311-0005).
                                    </span>
                                    Abgerufen unter:
                                    <a href="https://www-genesis.destatis.de/genesis/online/data?operation=find&suchanweisung_language=de&query=21311-0005">
                                        https://www-genesis.destatis.de/&#8203;genesis/&#8203;online/&#8203;data?operation=find&suchanweisung_language=de&query=21311-0005
                                    </a>.
                                </li>
                                <li>
                                  Statistisches Bundesamt (2019).
                                  <span style="font-style: italic;">
                                      BAföG - Geförderte Personen und finanzieller Aufwand: Bundesländer, Jahre, Personenkreis (21411-0002).
                                  </span>
                                  Abgerufen unter:
                                  <a href="https://www-genesis.destatis.de/genesis/online/data?operation=find&suchanweisung_language=de&query=+21411-0002">
                                      https://www-genesis.destatis.de/&#8203;genesis/&#8203;online/&#8203;data?operation=find&suchanweisung_language=de&query=+21411-0002
                                  </a>.
                              </li>
                              <li>
                                  Iost, Oliver (2019).
                                  <span style="font-style: italic;">
                                      "Statistik: BAföG für Studierende" aus "Hintergrund: Die Geschichte des BAföG von 1972 bis heute".
                                  </span>
                                  Abgerufen unter:
                                  <a href="https://www.bafoeg-rechner.de/Hintergrund/geschichte.php#stat">
                                      https://www.bafoeg-rechner.de/&#8203;Hintergrund/&#8203;geschichte.php#stat
                                  </a>
                              </li>
                            </ul>
                        </div>
                      </div>
                      <br>
                      <br>
                      <div>
                           <div>
                               <i class="fas fa-award icon"></i>
                               Datenqualität
                           </div>
                           <br>
                           <div style="font-size:smaller">
                               Beide Datensätze stammen aus seriösen Quellen.
                               Insbesondere bei staatlichen Finanzdaten zum BAföG ist davon auszugehen, dass die Daten mit besonderer Sorgfalt erhoben wurden.
                               Beide Datensätze sind vollständig und weisen somit keine fehlenden Werte auf. Das Datenformat ist maschinenlesbar und liegt bereits in einem "tidy"-Format vor.
                               Hinsichtlich der Kompatibilität ist zu beachten, dass der BAföG-Datensatz sich auf das Kalenderjahr bezieht, wohingegen die Studierendendaten jeweils nur für das Wintersemester erhoben wurden.
                               Das bedeutet, dass die Verknüpfung der beiden Datensätze mit einer Ungenauigkeit einhergeht, die sich weder gut bestimmen noch beheben lässt.
                         </div>
                       </div>
                </v-flex>
              </v-row>
            </v-sheet>
        </v-bottom-sheet >
        <br><br><br>

          <div style="text-align: justify">

              Im Ver&shy;lauf der Zeit ist ein Unter&shy;schied zwi&shy;schen den neu&shy;en und al&shy;ten Bun&shy;des&shy;län&shy;dern bei der För&shy;der&shy;quote er&shy;kennbar.
              Bei der Aus&shy;wahl des Studie&shy;nortes bleibt die Mehr&shy;heit der Studie&shy;renden im ei&shy;genen Bundes&shy;land oder im Nach&shy;bar&shy;bun&shy;des&shy;land.
              Daher lässt sich ver&shy;muten, dass die niedri&shy;geren Ein&shy;kommen im Os&shy;ten für einen Teil der höhe&shy;ren Förder&shy;quote ver&shy;ant&shy;wortlich sind.
              Bei den west&shy;lichen Bundes&shy;ländern bil&shy;den Schles&shy;wig-Holstein, Saar&shy;land, Nieder&shy;sachsen und Rhein&shy;land-Pfalz die Schluss&shy;lichter bei den durch&shy;schnitt&shy;lichen Ein&shy;kommen.
              Dies spie&shy;gelt sich bei den BAföG-Förder&shy;quoten ent&shy;sprechend eben&shy;falls wieder, be&shy;sonders ab etwa 2005.
              Das Saar&shy;land bil&shy;det hierbei eine Auß&shy;nahme auf&shy;grund der Grö&shy;ße des Bundes&shy;landes.
              Es gibt hier nur eine Uni&shy;ver&shy;sität und dem&shy;ent&shy;spre&shy;chend ver&shy;hältnis&shy;mäßig wenig Studie&shy;rende.
              Aus&shy;reißer können hier&shy;bei stärker ins Ge&shy;wicht fal&shy;len.


        </div>

        <br>
        <v-bottom-sheet v-model="sheet_ost">
          <template v-slot:activator="{ on }">
            <v-btn
              v-on="on"
            >
              <i class="fas fa-plus" style="margin-right: 10px"></i>Quellen
            </v-btn>
          </template>

          <v-sheet class="pa-10">
              <i class="fas fa-times fa-lg float-right" @click="sheet_ost = !sheet_ost"></i>
            <v-row >
                <v-flex xs12 md4 class="pa-3">
                </v-flex>
               <v-flex xs12 md4 class="pa-3">
                   <div>
                       <div>
                           <i class="fas fa-file-alt icon"></i>
                           Textquellen
                       </div>
                       <br>
                       <p style="font-size:smaller">
                           <ul>
                               <li>
                                 Destatis: Statistisches Bundesamt (2019).
                                 <span style="font-style: italic;">
                                     Statistisches Jahrbuch 2019 - Kapitel 14 Verdienste und Arbeitskosten.
                                 </span>
                                 Abgerufen unter:
                                 <a href="https://www.destatis.de/DE/Themen/Querschnitt/Jahrbuch/jb-verdienste-arbeitskosten.html">
                                     https://www.destatis.de/&#8203;DE/&#8203;Themen/&#8203;Querschnitt/&#8203;Jahrbuch/&#8203;jb-verdienste-arbeitskosten.html
                                 </a>.
                              </li>
                               <li>
                                 Kultusministerkonferenz (2019).
                                 <span style="font-style: italic;">
                                     Die Mobilität der Studienanfänger/-innen und Studierenden in Präsenzstudiengängen an Hochschulen in Trägerschaft der Länder in Deutschland 2017
                                 </span>
                                 Abgerufen unter:
                                 <a href="https://www.kmk.org/dokumentation-statistik/statistik/hochschulstatistik/die-mobilitaet-der-studienanfaenger-und-studierenden.html
                   ">
                                    https://www.kmk.org/&#8203;dokumentation-statistik/&#8203;statistik/&#8203;hochschulstatistik/&#8203;die-mobilitaet-der-studienanfaenger-und-studierenden.html
                                </a>.
                              </li>
                              <li>
                                Link, Bettina (2010).
                                <span style="font-style: italic;">
                                    Wanderungsverhalten von Studierenden zwischen den alten und den neuen Bundesländern. 20 Jahre Deutsche Einheit.
                                </span>
                                Statistische Monatshefte Rheinland-Pfalz.
                                Abgerufen unter:
                                <a href="https://www.statistik.rlp.de/fileadmin/dokumente/monatshefte/2010/September/09-2010-715.pdf">
                                    https://www.statistik.rlp.de/&#8203;fileadmin/&#8203;dokumente/&#8203;monatshefte/&#8203;2010/&#8203;September/&#8203;09-2010-715.pdf
                                </a>.
                             </li>
                             <li>
                               Heine, Christoph; Willich, Julia und Schneider, Heidrun (2009).
                               <span style="font-style: italic;">
                                   Informationsverhalten und Hochschulwahl von Studienanfängern in West- und Ostdeutschland.
                               </span>
                               Abgerufen unter:
                               <a href="http://docplayer.org/74106356-Informationsverhalten-und-hochschulwahl-von-studienanfaengern-in-west-und-ostdeutschland.html">
                                   http://docplayer.org/74106356-Informationsverhalten-und-hochschulwahl-von-studienanfaengern-in-west-und-ostdeutschland.html
                               </a>.
                            </li>
                          </ul>
                       </p>
                  </div>
               </v-flex>
               <v-flex xs12 md4 class="pa-3">
              </v-flex>
            </v-row>
          </v-sheet>
      </v-bottom-sheet>

    </div>
</template>

<style type="text/css">

/* ########################## */
/* Interactive slider*/

#dev-y-axis {
    font-weight: bold;
    text-align: left;
    position: relative;
    top: 180px;
    transform-origin: 0 0;
    transform: rotate(-90deg);
    font-size: 15px;
}

.year-texts {
    font-size: 15px;
    text-align: justify;
}

.perc {
    position: relative;
    left: 8%;
    top: 14px;
}

#dev_graph .descr {
    border-top: 0.1px dotted #B3E5FC;
    position: relative;
    left: 12% !important;
    width: 84% !important;
    margin: 0 1vw !important;
}

#slider-dev .v-input.theme--light.v-input__slider {
    width: 88% !important;
    position: relative;
    left: 12%;
    padding: 1vw;
}

#spark_dev {
    width: 88%;
    position: relative;
    left: 12%;
    margin-top: -120px;
}

/* Optimized above 1050px*/
@media (min-width: 0px) and (max-width: 1150px) {
    #perc20 {
        text-align: left;
        position: relative; top: 0.1vw;
    }
    #perc25 {
        text-align: left;
        position: relative; top: -2.05vw;
    }
    #perc30 {
        text-align: left;
        position: relative; top: -4.2vw;
    }
}
@media (min-width: 1150px) and (max-width: 1250px) {
    #perc20 {
        text-align: left;
        position: relative; top: 0.1vw;
    }
    #perc25 {
        text-align: left;
        position: relative; top: -1.85vw;
    }
    #perc30 {
        text-align: left;
        position: relative; top: -3.8vw;
    }
}
@media (min-width: 1250px) and (max-width: 1340px) {
    #perc20 {
        text-align: left;
        position: relative; top: 0.1vw;
    }
    #perc25 {
        text-align: left;
        position: relative; top: -1.7vw;
    }
    #perc30 {
        text-align: left;
        position: relative; top: -3.5vw;
    }
}
@media (min-width: 1340px) and (max-width: 1400px) {
    #perc20 {
        text-align: left;
        position: relative; top: 0.6vw;
    }
    #perc25 {
        text-align: left;
        position: relative; top: -1.3vw;
    }
    #perc30 {
        text-align: left;
        position: relative; top: -3.2vw;
    }
}
@media (min-width: 1400px) and (max-width: 1500px) {
    #perc20 {
        text-align: left;
        position: relative; top: 1.2vw;
    }
    #perc25 {
        text-align: left;
        position: relative; top: -0.8vw;
    }
    #perc30 {
        text-align: left;
        position: relative; top: -2.8vw;
    }
}
@media (min-width: 1500px) and (max-width: 1650px) {
    #perc20 {
        text-align: left;
        position: relative; top: 1.8vw;
    }
    #perc25 {
        text-align: left;
        position: relative; top: -0.8vw;
    }
    #perc30 {
        text-align: left;
        position: relative; top: -2.5vw;
    }
}

@media (min-width: 1650px) {
    #spark_dev {
        width: 735px;
        height: 172px;
        left: 85px;
    }
    #perc20 {
        text-align: left;
        position: relative; top: 1.8vw;
    }
    #perc25 {
        text-align: left;
        position: relative; top: -0.8vw;
    }
    #perc30 {
        text-align: left;
        position: relative; top: -2.5vw;
    }
    #dev_graph .descr {
        left: 85px!important;
        width: 700px !important;
    }
    #dev_graph  .v-input.theme--light.v-input__slider {
        position: relative;
        left: 85px;
        width: 735px !important;
    }
    #dev_graph {
        display: inline-block;
        margin: 0 auto;
    }
}


/*###########################################################*/

.year-texts {
    font-size: 15px;
    text-align: left;
}

.legend {
    font-size: 10px;
    margin: 10px 0px 20px;
}
.legend-color {
    position: relative;
    top: 7px;
    margin-right: 5px;
    height: 20px;
    width: 20px;
    border: 1px solid black;
    display: inline-block;
}

@media (max-width: 599px) {
    .legend div {
        display: inline-block;
        margin: 0px 0px 10px 20px;
    }
    .legend .legend-color {
        display: inline-block;
        margin: 0px 5px 0px 0px;
    }
}


</style>

<script>
const yearsShort = [
          '98',
          '99',
          '00',
          '01',
          '02',
          '03',
          '04',
          '05',
          '06',
          '07',
          '08',
          '09',
          '10',
          '11',
          '12',
          '13',
          '14',
          '15',
          '16',
          '17',
          '18'
          ]


export default {
  name: 'Development',

  data: () => ({
    sheet_ost: false,
    sheet: false,
    verlauf: false,
    slider: {
        value: 0,
        ticksLabels: yearsShort
    },
    percOverYears: {
      width: 1,
      radius: 1,
      padding: 8,
      lineCap: 'round',
      gradient: ['#0288D1','#81D4FA'],
      value: [0.2057649, 0.2121235, 0.2163986, 0.2447376, 0.2637627, 0.2715762, 0.2896462, 0.2917449, 0.2877431, 0.2895423, 0.2857571, 0.2933107, 0.3014509, 0.3041941, 0.3026595, 0.2875919, 0.2719744, 0.2528966, 0.2383740, 0.2252972, 0.2092836],
      gradientDirection: 'top',
      fill: false,
      type: 'trend',
      autoLineWidth: false,
      labels: yearsShort,
      labelSize: 3
    }
  }),
  methods: {
    onScrollVerlauf () {
        this.verlauf = true
    }
  },
  mounted: () => {
  }
};
</script>
