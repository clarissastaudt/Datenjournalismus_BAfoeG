<template>
    <div style="text-align: center;">
        <h1 class="font-weight-light light-blue--text" style="text-align:center">Wer bekommt wie viel Geld?</h1>
        <br>
        <v-layout id="counter">
            <v-row>
            <v-flex
              md4 sm6 xs12>
                  <div class="box-statistic" id="boxs1">
                     <h2 >
                         <countTo  ref='countbafogaverage' :startVal='countbafogaverage.startVal' :endVal='countbafogaverage.endVal' :duration='3000'>
                         </countTo>€
                     </h2>
                     <div style="font-size: smaller">haben 2018 BAföG-Empfänger*innen im Schnitt monatlich erhalten</div>
                 </div>
             </v-flex>
             <v-flex
               md4 sm6 xs12>
              <div class="box-statistic" id="boxs2">
                      <h2 >
                          <countTo  ref='countbafog' :startVal='countBafog.startVal' :endVal='countBafog.endVal' :duration='3000'>
                          </countTo>€
                      </h2>
                      <div style="font-size: smaller">beträgt der BAföG-Höchstsatz im Jahr 2019</div>
             </div>
              </v-flex>
            <v-flex md4 xs12>
                <div class="box-statistic" id="boxs3">
                    <h2 >
                        <countTo ref='countkosten' :startVal='countKosten.startVal' :endVal='countKosten.endVal' :duration='3000' separator=".">
                        </countTo>€
                    </h2>
                    <div style="font-size: smaller">hat der Staat 2018 für studentisches BAföG ausgegeben</div>
                </div>
            </v-flex>
        </v-row>
         </v-layout>
         <br>
         <br>

         <div id="av_monthly_graph">
             <h3 style="text-align:center; width: 93%; position: relative; left: 7%">
                 Mittlerer monatlicher BAföG-Satz im Zeitverlauf
             </h3>
             <div id="money-y-axis">
                 Mittlerer monatlicher BAföG-Satz
             </div>
             <div>
                 <div id="av500">
                     <div class="av">500€</div>
                     <hr class="descr">
                 </div>
                 <div id="av450">
                     <div class="av">450€</div>
                     <hr class="descr">
                 </div>
                 <div id="av400">
                     <div class="av">400€</div>
                     <hr class="descr">
                 </div>
                 <div id="av350">
                     <div class="av">350€</div>
                     <hr class="descr">
                 </div>
             </div>

                 <v-sparkline
                    id = "spark"
                    :label-size="averageMonthly.labelSize"
                    :value="averageMonthly.value"
                    :gradient="averageMonthly.gradient"
                    :smooth="averageMonthly.radius || false"
                    :padding="averageMonthly.padding"
                    :line-width="averageMonthly.width"
                    :stroke-linecap="averageMonthly.lineCap"
                    :gradient-direction="averageMonthly.gradientDirection"
                    :fill="averageMonthly.fill"
                    :type="averageMonthly.type"
                    :auto-line-width="averageMonthly.autoLineWidth"
                    auto-draw
                    auto-draw-duration=3000
                    height="100"
                ></v-sparkline>

            <v-slider
                v-model="slider_money.value"
                step="5"
                ticks="always"
                tick-size="5"
                :tick-labels="slider_money.ticksLabels"
            ></v-slider>
        </div>
        <div style="font-weight: bold;  font-size: 15px;">Jahr</div>
        <br>

        <div>
            <v-flex md8 sm12 offset-md2 v-if="slider_money.value==0">
                <h2>1998</h2>
                <p class="year-texts">
                    In den 90er Jah&shy;ren wer&shy;den im&shy;mer we&shy;niger Stu&shy;die&shy;rende durch BAföG ge&shy;för&shy;dert und auch die Gesamt&shy;aus&shy;ga&shy;ben für das BAföG sin&shy;ken ab.
                    Gleich&shy;zei&shy;tig stei&shy;gen die Lebens&shy;hal&shy;tungs&shy;kos&shy;ten b&shy;eson&shy;ders in den Groß&shy;städten deut&shy;lich.
                    Die Er&shy;höh&shy;ung des BAföG-Höchst&shy;satzes 1998 von 995 Mark um 15 Mark auf 1.010 Mark mo&shy;nat&shy;lich wird da&shy;her durch das Deu&shy;tsche Stu&shy;den&shy;ten&shy;werk als un&shy;zu&shy;rei&shy;chend kri&shy;ti&shy;siert.
                </p>
            </v-flex>
            <v-flex md8 sm12 offset-md2 v-if="slider_money.value==5">
                <h2>1999</h2>
                <p class="year-texts">
                    Für die mitt&shy;lere mo&shy;nat&shy;liche Förder&shy;höhe er&shy;ge&shy;ben sich nur g&shy;ering&shy;fügige Än&shy;de&shy;rungen.
                    Die neu ge&shy;wählte rot-grüne Bundes&shy;re&shy;gierung kün&shy;digt eine grund&shy;le&shy;gende BAföG-Reform an.
                </p>
            </v-flex>
            <v-flex md8 sm12 offset-md2 v-if="slider_money.value==10">
                <h2>2000</h2>
                <p class="year-texts">
                     Die im letz&shy;ten Jahr durch die rot-grüne Re&shy;gie&shy;rung an&shy;ge&shy;kündigte Reform führt zu ver&shy;schie&shy;denen Dis&shy;kuss&shy;ionen.
                     Letzt&shy;endlich fin&shy;det aber kei&shy;ne grund&shy;le&shy;gende Reform statt.
                </p>
            </v-flex>
            <v-flex md8 sm12 offset-md2 v-if="slider_money.value==15">
                <h2>2001</h2>
                <p class="year-texts">
                    Im Jahr 2001 wer&shy;den durch die rot-grünen Bun&shy;des&shy;re&shy;gie&shy;rung Ände&shy;rungen am BAföG be&shy;schlossen.
                    Die Aus&shy;ge&shy;stal&shy;tung zeigt, dass nun nach dem PISA-Schock der bil&shy;dungs&shy;po&shy;litische Aspekt gegen&shy;über finanz&shy;po&shy;li&shy;tischen Ge&shy;dan&shy;ken wie&shy;der an Ge&shy;wicht ge&shy;winnt.
                    Die För&shy;der&shy;be&shy;träge in Ost- und West&shy;deutsch&shy;land wer&shy;den an&shy;ge&shy;glichen und der Höchst&shy;satz steigt auf 1105 Mark.
                </p>
            </v-flex>
            <v-flex md8 sm12 offset-md2 v-if="slider_money.value==20">
                <h2>2002</h2>
                <p class="year-texts">
                    Im Ok&shy;tober 2002 wer&shy;den Förder- und Frei&shy;beträge für neue BAföG-An&shy;träge leicht ge&shy;hoben und so auf gerade Euro-Be&shy;träge an&shy;ge&shy;passt.
                </p>
            </v-flex>
            <v-flex md8 sm12 offset-md2 v-if="slider_money.value==25">
                <h2>2003</h2>
                <p class="year-texts">
                    Im Jahr 2003 be&shy;kom&shy;men etwa ein Vier&shy;tel der Stu&shy;die&shy;renden BAföG. Für den durch&shy;schnitt&shy;lichen Förder&shy;be&shy;trag er&shy;ge&shy;ben sich kei&shy;ne gro&shy;ßen Ver&shy;än&shy;de&shy;rungen.
                </p>
            </v-flex>
            <v-flex md8 sm12 offset-md2 v-if="slider_money.value==30">
                <h2>2004</h2>
                <p class="year-texts">
                    Im Jahr 2004 stei&shy;gen die BAföG-Aus&shy;gaben um 6% an, je&shy;doch bleibt der Anteil der voll&shy;ge&shy;för&shy;derten Stu&shy;dier&shy;enden wie auch im Vor&shy;jahr bei rund 37%.
                    Auch die durch&shy;schnitt&shy;liche Höhe der mo&shy;nat&shy;lichen Förder&shy;beträge pro Person ist nahe&shy;zu un&shy;ver&shy;ändert.
                </p>
            </v-flex>
            <v-flex md8 sm12 offset-md2 v-if="slider_money.value==35">
                <h2>2005</h2>
                <p class="year-texts">
                    Im Jahr 2005 liegt die Quote der Voll&shy;för&shy;derung bei 38%.
                    Am Anfang des Jahres wird das bundes&shy;weite Studien&shy;ge&shy;bühren&shy;ver&shy;bot auf&shy;ge&shy;hoben.
                </p>
            </v-flex>
            <v-flex md8 sm12 offset-md2 v-if="slider_money.value==40">
                <h2>2006</h2>
                <p class="year-texts">
                    Seit 2001 gab es für BAföG keine sig&shy;ni&shy;fikanten Er&shy;höh&shy;ungen.
                    Auf der Grund&shy;la&shy;ge der Kosten&shy;ent&shy;wick&shy;lung wird eine BAföG-Er&shy;höhung be&shy;schlos&shy;sen, die je&shy;doch erst im Jahr 2008 wirk&shy;sam wird.
                </p>
            </v-flex>
            <v-flex md8 sm12 offset-md2 v-if="slider_money.value==45">
                <h2>2007</h2>
                <p class="year-texts">
                    Der BAföG-Beirat der Bundes&shy;re&shy;gierung for&shy;dert eine Er&shy;höhung der BAföG-Sätze um 10,3%.
                    Der Grund sind ge&shy;stie&shy;gene Prei&shy;se und Mie&shy;ten in den letz&shy;ten fünf Jah&shy;ren.
                    Zu&shy;sätzlich sind ab Herbst 2007 Ver&shy;besse&shy;rungen für Stu&shy;dierende mit Kin&shy;dern ge&shy;plant.
                    Pro Kind sol&shy;len Stu&shy;die&shy;rende bis zum 10. Lebens&shy;jahr einen mo&shy;nat&shy;lichen BAföG-Zuschlag von 113 Euro er&shy;halten.
                </p>
            </v-flex>
            <v-flex md8 sm12 offset-md2 v-if="slider_money.value==50">
                <h2>2008</h2>
                <p class="year-texts">
                    2008 gibt es nach sie&shy;ben Jah&shy;ren end&shy;lich wieder eine BAföG-Er&shy;höhung.
                    Der Höchst&shy;satz wird auf 670 Euro mo&shy;nat&shy;lich an&shy;ge&shy;hoben.
                    Außer&shy;dem sind  Master&shy;studie&shy;rende nun bis zum 35. Lebens&shy;jahr BAföG-be&shy;rechtigt.
                </p>
            </v-flex>
            <v-flex md8 sm12 offset-md2 v-if="slider_money.value==55">
                <h2>2009</h2>
                <p class="year-texts">
                    Die Er&shy;höhung aus dem Vor&shy;jahr kommt zum Tra&shy;gen. Im Schnitt er&shy;hal&shy;ten die BAföG-Empfänger*innen nun 434 Euro im Monat.
                </p>
            </v-flex>
            <v-flex md8 sm12 offset-md2 v-if="slider_money.value==60">
                <h2>2010</h2>
                <p class="year-texts">
                    Dieses Jahr tritt das 23. BAföG-Än&shy;derungs&shy;gesetz in Kraft und ver&shy;ändert ei&shy;ni&shy;ge Be&shy;träge:
                    Die Frei&shy;beträge wer&shy;den um 3% an&shy;ge&shy;hoben und der Höchst&shy;satz steigt auf mo&shy;natlich 670 Euro.
                    Außer&shy;dem ist es mög&shy;lich durch BAföG ge&shy;fördert zu wer&shy;den, wenn man vor Voll&shy;endung des 35. Le&shy;bens&shy;jahrs ein Master&shy;studium beginnt.
                </p>
            </v-flex>
            <v-flex md8 sm12 offset-md2 v-if="slider_money.value==65">
                <h2>2011</h2>
                <p class="year-texts">
                    Das Deutschland&shy;sti&shy;pendium für be&shy;gabte Stu&shy;die&shy;rende wird ein&shy;ge&shy;führt.
                    Ein&shy;kom&shy;mens&shy;un&shy;ab&shy;hängig be&shy;trägt die För&shy;de&shy;rung 300 Euro im Monat.
                </p>
            </v-flex>
            <v-flex md8 sm12 offset-md2 v-if="slider_money.value==70">
                <h2>2012</h2>
                <p class="year-texts">
                    2012 gibt es keine be&shy;deu&shy;tenden Ver&shy;än&shy;derungen beim mitt&shy;leren mo&shy;nat&shy;li&shy;chen BAföG-Satz.
                </p>
            </v-flex>
            <v-flex md8 sm12 offset-md2 v-if="slider_money.value==75">
                <h2>2013</h2>
                <p class="year-texts">
                    Obwohl die Förder&shy;si&shy;tuation sich zum Vor&shy;jahr nicht stark ver&shy;ändert hat, können sich Bund und Län&shy;der nicht auf eine Ver&shy;än&shy;derung des BAföGs einigen.
                </p>
            </v-flex>
            <v-flex md8 sm12 offset-md2 v-if="slider_money.value==80">
                <h2>2014</h2>
                <p class="year-texts">
                    In die&shy;sem Jahr tritt das 25. BAföG-Än&shy;derungs&shy;ge&shy;setz in Kraft.
                    Der Bund über&shy;nimmt ab dem 1. Ja&shy;nuar 2015 die kom&shy;plette Fi&shy;nan&shy;zie&shy;rung des BAföGs.
                    Die ge&shy;plan&shy;te Er&shy;höhung um je&shy;weils 7% der Be&shy;darfs&shy;sätze der Bun&shy;des&shy;aus&shy;bil&shy;dungs&shy;för&shy;derung und der Ein&shy;kom&shy;mens&shy;frei&shy;be&shy;trä&shy;ge der El&shy;tern tre&shy;ten aber erst zum Win&shy;ter&shy;se&shy;mes&shy;ter 2016/2017 in Kraft.
                    Für je&shy;des ei&shy;gene Kind im ei&shy;genen Haus&shy;halt sol&shy;len Stu&shy;dieren&shy;de ab dann 130 Euro be&shy;kom&shy;men.
                    Beim Wohn&shy;geld&shy;zu&shy;schlag soll es zu einer Er&shy;höhung auf 250 Euro kom&shy;men.
                    Mini&shy;job-Ein&shy;kommen bis zu 450 Euro im Monat sol&shy;len an&shy;rechnungs&shy;frei wer&shy;den.
                    Ei&shy;ge&shy;nes Ver&shy;mö&shy;gen bis 7.500 Euro wird eben&shy;falls nicht mehr be&shy;rück&shy;sichtigt.
                    Ab&shy;schlä&shy;ge von 80 Pro&shy;zent von der vo&shy;raus&shy;sichtli&shy;chen BAföG-För&shy;de&shy;rung kön&shy;nen bei Erst&shy;an&shy;trägen ab Au&shy;gust 2015 aus&shy;ge&shy;zahlt wer&shy;den, sol&shy;lte die kurz&shy;fris&shy;ti&shy;ge Be&shy;ar&shy;bei&shy;tung nicht mög&shy;lich sein.
                    Eine För&shy;de&shy;rung ist nun auch zwi&shy;schen dem Abschluss eines Ba&shy;chelors und eines nach&shy;fol&shy;genden Master&shy;stu&shy;diums mög&shy;lich, wenn eine vor&shy;läu&shy;fige Zu&shy;lassung zum Master&shy;studium vor&shy;liegt.
                </p>
            </v-flex>
            <v-flex md8 sm12 offset-md2 v-if="slider_money.value==85">
                <h2>2015</h2>
                <p class="year-texts">
                    Die BAföG-Fi&shy;nan&shy;zierung wird nun voll&shy;ständig durch den Bund ge&shy;tra&shy;gen.
                    Wenn Erst&shy;an&shy;träge von Stu&shy;die&shy;renden kurz&shy;zeitig nicht be&shy;ar&shy;beitet wer&shy;den kön&shy;nen, besteht zum ersten Mal die Mög&shy;lich&shy;keit eine BAföG-Voraus&shy;zah&shy;lung zu er&shy;hal&shy;ten.
                </p>
            </v-flex>
            <v-flex md8 sm12 offset-md2 v-if="slider_money.value==90">
                <h2>2016</h2>
                <p class="year-texts">
                    Zum 1.1.2016 kön&shy;nen aus&shy;ländi&shy;sche Stu&shy;dierende mit Auf&shy;ent&shy;halts&shy;be&shy;rech&shy;tigung oder als Ge&shy;duldete aus Dritt&shy;staa&shy;ten be&shy;reits nach 15 Mo&shy;naten Auf&shy;ent&shy;halt eine BAföG-För&shy;derung er&shy;hal&shy;ten.
                    Die Be&shy;darfs&shy;sätze wer&shy;den, wie 2014 ge&shy;plant, zum Winter&shy;se&shy;mester 2016/2017 um 7% erhöht. Auch Wohn- und Kinder&shy;zu&shy;schläge stei&shy;gen. Mini&shy;jobs bis 450 Euro wer&shy;den an&shy;rech&shy;nungs&shy;frei.
                </p>
            </v-flex>
            <v-flex md8 sm12 offset-md2 v-if="slider_money.value==95">
                <h2>2017</h2>
                <p class="year-texts">
                    Das Kinder&shy;geld wird um 2 Euro an&shy;ge&shy;hoben.
                    Eine Be&shy;frei&shy;ung vom Rund&shy;funk&shy;bei&shy;trag kann für BAföG-Empfänger*innen nun bis zu drei Jah&shy;re rück&shy;wirkend be&shy;antragt werden.
                </p>
            </v-flex>
            <v-flex md8 sm12 offset-md2 v-if="slider_money.value==100">
                <h2>2018</h2>
                <p class="year-texts">
                    Der mitt&shy;lere monat&shy;liche BAföG-Satz sinkt wieder.
                    Die Gesamt&shy;jahres&shy;ausgaben für das stu&shy;dentische BAföG fal&shy;len von 2,2 Milliarden Euro auf 2,0 Milliarden Euro.
                </p>
            </v-flex>

        </div>

        <v-bottom-sheet v-model="sheet_money">
          <template v-slot:activator="{ on }">
            <v-btn
              v-on="on"
            >
              <i class="fas fa-plus" style="margin-right: 10px"></i>Methodik
            </v-btn>
          </template>

          <v-sheet class="pa-10">


              <i class="fas fa-times fa-lg float-right" @click="sheet_money = !sheet_money"></i>


            <v-row>
                <v-flex xs12 md4 class="pa-3">
                    <div>
                        <i class="fas fa-list-ol icon"></i>
                        Beschreibung der Datenanalyse
                    </div>
                    <br>
                    <div style="font-size:smaller">
                        <div>
                            <h4>Wie kommt der BAföG-Höchstsatz zu Stande?</h4>
                            <p>
                                 Der BAföG-Höchstsatz bezeichnet die Summe aus dem Grundbedarf, der Wohnpauschale und dem Zuschlag zur Kranken- bzw. Pflegeversicherung.
                                 Die maximale Wohnpauschale wird nur dann gewährt, wenn Studierende nicht mehr bei ihren Eltern wohnen.
                                 Einen Kranken- bzw. Pflegeversicherungszuschlag erhält, wer sich bereits selbst versichern muss, z.B. weil er über 25 Jahre alt ist.
                                 Im Wintersemester 2019/2020 liegt der Höchstsatz bei 853 Euro.
                            </p>

                            <table>
                                <tr><td></td><td>Grundbedarf</td><td>419€</td></tr>
                                <tr><td>+</td><td>Wohnpauschale</td><td>325€</td></tr>
                                <tr><td>+</td><td>Kranken- und Pflegeversicherungszuschlag </td><td>109€</td></tr>
                                <tr class="border_top"><td>=</td><td>BAföG-Höchstsatz</td><td>853€</td></tr>
                            </table>
                        </div>
                        <br>
                        <div>
                            <h4>Wie werden die Gesamtkosten bestimmt?</h4>
                            <p>
                               Das statistische Bundesamt erhebt jedes Jahr auf Bundeslandsebene den finanziellen Aufwand für die studentische BAföG-Förderung. Dies erlaubt durch einfache Addition die Bestimmung der Gesamtkosten.
                           </p>
                       </div>
                       <div>
                           <h4>Wie wird die durchschnittliche monatliche Förderhöhe berechnet?</h4>
                           <p>
                                Oft werden Studierende nicht über das gesamte Kalenderjahr gefördert, z.B. wenn sich ihre oder die finanzielle Situation der Eltern verändert, sie ihr Studium beenden oder sie wieder zu Hause wohnen.
                                Insofern greift eine Division der Gesamtkosten durch die Anzahl der Geförderten zu kurz.
                                Stattdessen teilt man die Gesamtkosten der BAföG-Förderung durch den durchschnittlichen Monatsbestand an Studierenden.
                                Der durchschnittliche Monatsbestand ist direkt im BAföG Datensatz enthalten und gibt die durschnittliche Anzahl der geförderten Studierenden pro Monat wieder.
                            </p>
                        </div>
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
                                  Bundesministerium für Bildung und Forschung (2020).
                                 <span style="font-style: italic;">
                                     Häufig gestellte Fragen
                                 </span>
                                 Abgerufen unter:
                                 <a href="https://www.deutschlandstipendium.de/de/haeufig-gestellte-fragen-1729.html">
                                     https://www.deutschlandstipendium.de/&#8203;de/&#8203;haeufig-gestellte-fragen-1729.html
                                 </a>.
                              </li>
                              <li>
                                 Bundesministerium für Bildung und Forschung (2020).
                                <span style="font-style: italic;">
                                    Infografiken
                                </span>
                                Abgerufen unter:
                                <a href="https://www.deutschlandstipendium.de/de/infografiken-1736.html">
                                    https://www.deutschlandstipendium.de/&#8203;de/&#8203;infografiken-1736.html
                                </a>.
                             </li>
                               <li>
                                   Bundesministerium für Bildung und Forschung (2019).
                                   <span style="font-style: italic;">
                                       Das BAföG: alle Infos auf einen Blick. Fragen und Antworten.
                                    </span>
                                    Abgerufen unter:
                                   <a href="https://www.xn--bafg-7qa.de/de/welche-bedarfssaetze-sieht-das-bafoeg-vor--375.php">
                                       https://www.xn--bafg-7qa.de/&#8203;de/&#8203;welche-bedarfssaetze-sieht-das-bafoeg-vor--375.php
                                   </a>.
                               </li>
                               <li>
                                 Bundesverfassungsgericht (2005).
                                 <span style="font-style: italic;">
                                    Urteil des Zweiten Senats vom 26. Januar 2005.
                                 </span>
                                 Abgerufen unter:
                                 <a href="https://www.bundesverfassungsgericht.de/SharedDocs/Entscheidungen/DE/2005/01/fs20050126_2bvf000103.html">
                                     https://www.bundesverfassungsgericht.de/&#8203;SharedDocs/&#8203;Entscheidungen/&#8203;DE/&#8203;2005/&#8203;01/&#8203;fs20050126_&#8203;2bvf000103.html
                                 </a>.
                              </li>
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
                               Deutsches Studentenwerk (2017).
                               <span style="font-style: italic;">
                                   Studie: BAföG-Bedarfssätze zu niedrig.
                               </span>
                               Abgerufen unter:
                               <a href="https://www.studentenwerke.de/de/content/studie-baf%C3%B6g-bedarfss%C3%A4tze-zu-niedrig">
                                   https://www.studentenwerke.de/&#8203;de/&#8203;content/&#8203;studie-baf%C3%B6g-bedarfss%C3%A4tze-zu-niedrig
                               </a>.
                            </li>
                            <li>
                                Deutsches Studentenwerk (?).
                              <span style="font-style: italic;">
                                  Studienfinanzierung. Geschichte und Statistik zum BAföG.
                              </span>
                              Abgerufen unter:
                              <a href="https://www.studentenwerke.de/de/content/geschichte-und-statistik-zum-baf%C3%B6g">
                                  https://www.studentenwerke.de/&#8203;de/&#8203;content/&#8203;geschichte-und-statistik-zum-baf%C3%B6g
                              </a>.
                           </li>
                           <li>
                             Gala, Olga (2014).
                             <span style="font-style: italic;">
                                 Bafög. Weniger Studenten bekommen Förderung.
                             </span>
                             Abgerufen unter:
                             <a href="https://www.zeit.de/karriere/2014-08/bafoeg-empfaenger-infografik">
                                 https://www.zeit.de/&#8203;karriere/&#8203;2014-08/&#8203;bafoeg-empfaenger-infografik
                             </a>.
                          </li>
                            <li>
                              Geil, Karin (2007).
                              <span style="font-style: italic;">
                                  Ausbildungsförderung. Sparkur für Studenten.
                              </span>
                              Abgerufen unter:
                              <a href="https://www.zeit.de/campus/online/2007/07/bafoeg">
                                  https://www.zeit.de/&#8203;campus/&#8203;online/&#8203;2007/&#8203;07/&#8203;bafoeg
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
                                Rhein Zeitung (1998).
                                <span style="font-style: italic;">
                                   Bafög erhöht. Bundesregierung beschließt leichte Anhebung noch 1998.
                                </span>
                                Abgerufen unter:
                                <a href="https://archiv.rhein-zeitung.de/on/98/01/13/topnews/bafoeg.html">
                                    https://archiv.rhein-zeitung.de/&#8203;on/&#8203;98/&#8203;01/&#8203;13/&#8203;topnews/&#8203;bafoeg.html
                                </a>.
                             </li>
                             <li>
                               Spiegel Panorama (2010).
                               <span style="font-style: italic;">
                                   Mehr Geld für Studenten. Bafög-Höchstsatz soll auf 670 Euro steigen.
                               </span>
                               Abgerufen unter:
                               <a href="https://www.spiegel.de/lebenundlernen/uni/mehr-geld-fuer-studenten-bafoeg-hoechstsatz-soll-auf-670-euro-steigen-a-687887.html">
                                   https://www.spiegel.de/&#8203;lebenundlernen/&#8203;uni/&#8203;mehr-geld-fuer-studenten-bafoeg-hoechstsatz-soll-auf-670-euro-steigen-a-687887.html
                               </a>.
                            </li>
                              <li>
                                Studis online (2005).
                                <span style="font-style: italic;">
                                    Hochschulpolitik. Immer mehr BAföG-EmpfängerInnen - aber wie lange noch?
                                </span>
                                Abgerufen unter:
                                <a href="https://www.studis-online.de/HoPo/art-311-bafoeg-statistik-2004.php">
                                    https://www.studis-online.de/&#8203;HoPo/&#8203;art-311-bafoeg-statistik-2004.php
                                </a>.
                             </li>
                             <li>
                               Studis online (2017).
                               <span style="font-style: italic;">
                                   Jahresausblick. Finanzielle Änderungen für Studenten 2017.
                               </span>
                               Abgerufen unter:
                               <a href="https://www.bafoeg-rechner.de/Hintergrund/art-1991-jahresausblick-2017.php">
                                   https://www.bafoeg-rechner.de/&#8203;Hintergrund/&#8203;art-1991-jahresausblick-2017.php
                               </a>.
                            </li>
                             <li>
                                Timmermann, Dieter (2014).
                               <span style="font-style: italic;">
                                   Stärkt das Bafög!
                               </span>
                               Abgerufen unter:
                               <a href="https://www.sueddeutsche.de/bildung/studienfinanzierung-staerkt-das-bafoeg-1.1749188">
                                   https://www.sueddeutsche.de/&#8203;bildung/&#8203;studienfinanzierung-staerkt-das-bafoeg-1.1749188
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
                                  <a href="https://github.com/clarissastaudt/Datenjournalismus_BAfoeG/blob/master/R/money.R">
                                      https://github.com/&#8203;clarissastaudt/&#8203;Datenjournalismus_BAfoeG/&#8203;blob/&#8203;master/&#8203;R/&#8203;money.R
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
                                    BAföG - Geförderte Personen und finanzieller Aufwand: Bundesländer, Jahre, Personenkreis (21411-0002).
                                </span>
                                Abgerufen unter:
                                <a href="https://www-genesis.destatis.de/genesis/online/data?operation=find&suchanweisung_language=de&query=+21411-0002">
                                    https://www-genesis.destatis.de/&#8203;genesis/&#8203;online/&#8203;data?operation=find&suchanweisung_language=&#8203;de&query=&#8203;+21411-0002
                                </a>.
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
                       <div style="font-size: smaller">
                           Der Datensatz stammt aus seriöser Quelle.
                           Insbesondere bei staatlichen Finanzdaten zum BAföG ist davon auszugehen, dass die Daten mit besonderer Sorgfalt erhoben wurden.
                           Der Datensatz ist vollständig und weist somit keine fehlenden Werte auf. Das Datenformat ist maschinenlesbar und liegt bereits in einem "tidy"-Format vor.
                     </div>
                   </div>
              </v-flex>
            </v-row>
          </v-sheet>
      </v-bottom-sheet>

      <br><br><br>


        <div style="text-align: justify">
            <p>
                Auf die Hö&shy;he der durch&shy;schnittlichen mo&shy;nat&shy;lichen För&shy;de&shy;rung der Stu&shy;die&shy;ren&shy;den haben die Re&shy;for&shy;men einen di&shy;rekten und gut sicht&shy;ba&shy;ren Ein&shy;fluss.
                Das be&shy;deutet, dass Er&shy;höhun&shy;gen der Frei- und Förder&shy;be&shy;träge sich auch tat&shy;sächlich im Geld&shy;beutel der Ge&shy;för&shy;der&shy;ten nieder&shy;schla&shy;gen.
            </p>
            <p>
                Ob das aus&shy;reichend ist, lässt sich nicht di&shy;rekt aus der Kur&shy;ve er&shy;sehen. Die Lebens&shy;haltungs&shy;kosten und das mitt&shy;lere Ein&shy;kommen in Deutsch&shy;land sind in dem ge&shy;zeig&shy;ten Zeit&shy;raum ge&shy;stie&shy;gen.
                Daher ist je&shy;de Pha&shy;se der Stag&shy;na&shy;tion oder gering&shy;fügi&shy;gen Ände&shy;rung beim BAföG sehr pr&shy;oble&shy;matisch für das zur Ver&shy;fügung stehende Ein&shy;kommen der Stu&shy;die&shy;renden.
                Genau wie bei Tarif&shy;ver&shy;handlungen müssten die Stei&shy;ge&shy;rungen die&shy;sen Effekten ent&shy;ge&shy;gen&shy;wirken.
                Ob dies der Fall ist, dürfte frag&shy;lich sein. Die Sozial&shy;erhe&shy;bungen des Deut&shy;schen Studenten&shy;werks er&shy;geben, dass ein Teil der Stu&shy;die&shy;renden ih&shy;ren Lebens&shy;unte&shy;rhalt mit Neben&shy;tätig&shy;keiten be&shy;strei&shy;ten muss.
                Auch wird fest&shy;ge&shy;stellt, dass die För&shy;derung zu niedrig ist und selbst mit BAföG zu wenig Geld für den Lebens&shy;unterhalt zur Ver&shy;fügung steht.
            </p>
            <p>
                Das Durch&shy;schnitts&shy;ein&shy;kommen ist aller&shy;dings kein guter Indi&shy;kator, um zu er&shy;kennen, wie viel Geld die meisten Studie&shy;renden er&shy;halten, da die Streu&shy;ung der Förder&shy;beträge nicht be&shy;rück&shy;sichtigt ist.
                Der Me&shy;dian wäre hier&shy;für ein besseres Maß, lässt sich aller&shy;dings nicht mit den vor&shy;han&shy;denen Daten er&shy;mitteln.
            </p>
        </div>

        <div style="text-align: center !important">
            <v-bottom-sheet v-model="sheet_money_text">
              <template v-slot:activator="{ on }">
                <v-btn
                  v-on="on"
                >
                  <i class="fas fa-plus" style="margin-right: 10px"></i>Quellen
                </v-btn>
              </template>

              <v-sheet class="pa-10">
                  <i class="fas fa-times fa-lg float-right" @click="sheet_money_text = !sheet_money_text"></i>
                <v-row>
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
                                        Deutsches Studentenwerk (2017).
                                       <span style="font-style: italic;">
                                           Studie: BAföG-Bedarfssätze zu niedrig.
                                        </span>
                                        Abgerufen unter:
                                       <a href="https://www.studentenwerke.de/de/content/studie-baf%C3%B6g-bedarfss%C3%A4tze-zu-niedrig">
                                           https://www.studentenwerke.de/&#8203;de/&#8203;content/&#8203;studie-baf%C3%B6g-bedarfss%C3%A4tze-zu-niedrig
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



    </div>



</template>

<style>

/* ########################## */
/* Interactive slider*/

#money-y-axis {
    font-weight: bold;
    text-align: left;
    position: relative;
    top: 230px;
    transform-origin: 0 0;
    transform: rotate(-90deg);
    font-size: 15px;
}

.year-texts {
    font-size: 15px;
    text-align: justify;
}

.av {
    position: relative;
    left: 4%;
    top: 14px;
}

#av_monthly_graph .descr {
    border-top: 0.1px dotted #B3E5FC;
    position: relative;
    left: 7% !important;
    width: 88% !important;
    margin: 0 1vw !important;
}

.v-input.theme--light.v-input__slider {
    width: 93% !important;
    position: relative;
    left: 7%;
    padding: 1vw;
}

#spark {
    width: 93%;
    position: relative;
    left: 7%;
    margin-top: -120px;
}

/* Optimized above 1050px*/
@media (min-width: 0px) and (max-width: 1250px) {
    #av350{
        text-align: left; position: relative; top: 1.89vw;
    }
    #av400 {
        text-align: left; position: relative; top: 0.29vw;
    }
    #av450 {
        text-align: left; position: relative; top: -1.31vw;
    }
    #av500 {
        text-align: left; position: relative; top: -2.91vw;
    }
}
@media (min-width: 1250px) and (max-width: 1250px) {
    #av350{
        text-align: left; position: relative; top: 3.79vw;
    }
    #av400 {
        text-align: left; position: relative; top: 1.59vw;
    }
    #av450 {
        text-align: left; position: relative; top: -0.61vw;
    }
    #av500 {
        text-align: left; position: relative; top: -2.81vw;
    }
}
@media (min-width: 1250px) and (max-width: 1300px) {
    #av350{
        text-align: left; position: relative; top: 3.59vw;
    }
    #av400 {
        text-align: left; position: relative; top: 1.69vw;
    }
    #av450 {
        text-align: left; position: relative; top: -0.31vw;
    }
    #av500 {
        text-align: left; position: relative; top: -2.31vw;
    }
}

@media (min-width: 1300px) and (max-width: 1450px) {
    #av350{
        text-align: left; position: relative; top: 3.59vw;
    }
    #av400 {
        text-align: left; position: relative; top: 1.69vw;
    }
    #av450 {
        text-align: left; position: relative; top: -0.21vw;
    }
    #av500 {
        text-align: left; position: relative; top: -2.11vw;
    }
    #money-y-axis {
        top: 245px;
    }
}

@media (min-width: 1450px) and (max-width: 1600px){
    #av350{
        text-align: left; position: relative; top: 4.55vw;
    }
    #av400 {
        text-align: left; position: relative; top: 2.43vw;
    }
    #av450 {
        text-align: left; position: relative; top: 0.31vw;
    }
    #av500 {
        text-align: left; position: relative; top: -1.81vw;
    }
}

@media (min-width: 1600px) and (max-width: 1850px) {
    #av350{
        text-align: left; position: relative; top: 5.65vw;
    }
    #av400 {
        text-align: left; position: relative; top: 3.23vw;
    }
    #av450 {
        text-align: left; position: relative; top: 0.81vw;
    }
    #av500 {
        text-align: left; position: relative; top: -1.61vw;
    }
}

@media (min-width: 1851px) {
    #spark {
        width: 870px;
        height: 270px;
        left: 40px;
    }
    #av350{
        text-align: left; position: relative; top: 5.25vw;
    }
    #av400 {
        text-align: left; position: relative; top: 3.03vw;
    }
    #av450 {
        text-align: left; position: relative; top: 0.81vw;
    }
    #av500 {
        text-align: left; position: relative; top: -1.41vw;
    }
    .descr {
        left: 70px!important;
        width: 770px !important;
    }
    .v-input.theme--light.v-input__slider {
        position: relative;
        left: 68px;
        width: 818px !important;
    }
    #av_monthly_graph {
        display: inline-block;
        margin: 0 auto;
    }
}

/* ########################## */

.box-statistic {
    border: 1px solid #b3e5fc;
    padding: 15px 20px 20px;
    height: 100%;
}

#boxs1 {
    margin: 0px 10px 0px 0px;
}
#boxs2 {
    margin: 0px 10px 0px 10px;
}
#boxs3 {
    margin: 0px 0px 0px 10px;
}

@media (max-width: 959px) {
    #boxs1 {
        margin: 0px 10px 0px 0px;
    }
    #boxs2 {
        margin: 0px 0px 0px 10px;
    }
    #boxs3 {
        margin: 20px 0px 0px 0px;
    }
}

@media (max-width: 599px) {
    #counter {
        margin-bottom: 15px;
    }
    #boxs1 {
        margin: 0px 0px 10px 0px;
    }
    #boxs2 {
        margin: 10px 0px 10px 0px;
    }
    #boxs3 {
        margin: 20px 0px 0px 0px;
    }
}

tr.border_top td {
  border-top: 1px solid #e0e0e0;
  padding-top: 5px;
}
td {
    padding-right: 10px;
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

    import countTo from 'vue-count-to'


    export default {
      name: 'Money',
      components: { countTo },

      data: () => ({
          slider_money: {
              value: 0,
              ticksLabels: yearsShort
          },
         sheet_money: false,
         sheet_money_text: false,
         countBafog: {
             startVal: 0,
             endVal: 853
         },
         countbafogaverage: {
             startVal: 0,
             endVal: 493
         },
         countKosten: {
             startVal: 0,
             endVal: 2001734000
         },
         averageMonthly: {
           width: 1,
           radius: 1,
           padding: 8,
           lineCap: 'round',
           gradient: ['#0288D1','#81D4FA'],
           value: [316.3879, 321.4173, 325.5221, 364.9660, 370.7308, 369.9536, 371.0620, 375.4148, 375.2258, 375.1466, 398.2331, 433.9942, 436.1962, 451.8813, 447.6898, 446.3513, 447.6670, 448.1998, 463.6219, 499.1927, 492.8651],
           gradientDirection: 'top',
           fill: false,
           type: 'trend',
           autoLineWidth: false,
           labels: yearsShort,
           labelSize: 5
         }
      }),
      methods: {
        onScrollGeld () {

            },
        },
        mounted: function () {
                /* eslint-disable no-console */
                let interval = window.setInterval(() => {
                    let el = document.getElementById("counter");
                    var r;
                    var html;
                    if ( !el || 1 !== el.nodeType ) { return false; }
                    html = document.documentElement;
                    r = el.getBoundingClientRect();

                    let inView = ( !!r
                      && r.bottom >= 0
                      && r.right >= 0
                      && r.top <= html.clientHeight
                      && r.left <= html.clientWidth
                    )

                    if (inView) {
                        this.$nextTick(function() {
                            this.$refs.countkosten.start();
                            this.$refs.countbafog.start();
                            this.$refs.countbafogaverage.start();
                            clearInterval(interval);
                        })
                    }
                }, 500)
            }
  }
    </script>
