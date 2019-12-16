<template>
    <div>
        <h1 class="font-weight-light light-blue--text" style="text-align:left">Wer bekommt wie viel Geld?</h1>
        <br>
        <v-layout id="counter">
            <v-flex
              xs4
             style="margin: 0px 10px 0px 0px;" class="box-statistic">
                 <h2 >
                     <countTo  ref='countbafogaverage' :startVal='countbafogaverage.startVal' :endVal='countbafogaverage.endVal' :duration='3000'>
                     </countTo>€
                 </h2>
                 <div style="font-size: smaller">haben 2018 BAföG-Empfänger*innen im Schnitt monatlich erhalten</div>
             </v-flex>
             <v-flex
               xs4
              style="margin: 0px 10px 0px 10px;" class="box-statistic">
                  <h2 >
                      <countTo  ref='countbafog' :startVal='countBafog.startVal' :endVal='countBafog.endVal' :duration='3000'>
                      </countTo>€
                  </h2>
                  <div style="font-size: smaller">beträgt der BAföG-Höchstsatz im Jahr 2019</div>
              </v-flex>
            <v-flex xs4 class="box-statistic" style="margin: 0px 0px 0px 10px">
                <h2 >
                    <countTo ref='countkosten' :startVal='countKosten.startVal' :endVal='countKosten.endVal' :duration='3000' separator=".">
                    </countTo>€
                </h2>
                <div style="font-size: smaller">hat der Staat 2018 für studentisches BAföG ausgegeben</div>
            </v-flex>
         </v-layout>
         <br>
         <h3 style="text-align:left; width: 93%; position: relative; left: 7%">Anteil der BAföG-Empfänger*innen mit Vollförderung</h3>
         <div style="font-weight: bold; text-align: left; position: relative; top: 250px;  transform-origin: 0 0; transform: rotate(-90deg); font-size: 15px;">Anteil der BAföG-Empfänger<br>mit Vollförderung</div>
         <v-img
           src="../assets/voll.png"
           contain
           style="width: 93%; position: relative; left: 7%;"
         ></v-img>
         <div style="font-weight: bold;  font-size: 15px; position: relative; top: -15px;">Jahr</div>
        <div>
            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
      </div>
        <br>



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


              <v-row align="left" justify="left">
                  <v-flex xs4 class="pa-3">
                      <div>
                          <i class="fas fa-list-ol icon"></i>
                          Vorgehen
                      </div>
                      <br>
                      <div style="font-size:smaller">
                          <h4>Wie werden die Gesamtkosten bestimmt?</h4>
                          <p>
                             Das statistische Bundesamt erhebt jedes Jahr auf Bundeslandsebene den finanziellen Aufwand für die studentische BAföG-Förderung. Dies erlaubt durch einfache Addition die Bestimmung der Gesamtkosten.
                         </p>
                         <h4>Wie wird die durchschnittliche monatliche Förderhöhe berechnet?</h4>
                         <p>
                              Oft werden Studierende nicht über das gesamte Kalenderjahr gefördert, z.B. wenn sich ihre oder die finanzielle Situation der Eltern verändert, sie ihr Studium beenden oder sie wieder zu Hause wohnen.
                              Hierzu dividiert man die Gesamtkosten der BAföG-Förderung durch den durchschnittlichen Monatsbestand an Studierenden.
                         <h4>Wie kommt der BAföG-Höchstsatz zu Stande?</h4>
                         <p>
                              Der BAföG-Höchstsatz bezeichnet die Summe aus dem Grundbedarf, der Wohnpauschale und dem Zuschlag zur Kranken- bzw. Pflegeversicherung.
                              Die maximale Wohnpauschale wird nur dann gewährt, wenn Studierende nicht mehr bei ihren Eltern wohnen.
                              Einen Kranken- bzw. Pflegeversicherungszuschlag erhält, wer sich bereits selbst versichern muss, z.B. weil er über 25 Jahre alt ist.
                         </p>

                         <table>
                             <tr><td></td><td>Grundbedarf</td><td>419€</td></tr>
                             <tr><td>+</td><td>Wohnpauschale</td><td>325€</td></tr>
                             <tr><td>+</td><td>Kranken- und Pflegeversicherungszuschlag </td><td>109€</td></tr>
                             <tr class="border_top"><td>=</td><td>BAföG-Höchstsatz</td><td>853€</td></tr>
                         </table>
                         <br>
                          <h4> Wie ermittelt man den Anteil der BAföG-Empfänger*innen mit Vollförderung?</h4>
                          <p>
                             Der Anteil der BAföG-Empfänger*innen mit Vollförderung ergibt sich aus der Division der Anzahl der Studierenden mit Vollförderung durch die Anzahl der BAföG-Empfänger*innen in einem Jahr.
                          </p>
                      </div>
                  </v-flex>
                 <v-flex xs4 class="pa-3">
                     <div>
                         <i class="fas fa-file-alt icon"></i>
                         Quellen
                     </div>
                     <br>
                     <p style="font-size:smaller">
                         Irgendwas zum Zeug in den Texten... Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                     </p>
                 </v-flex>
                 <v-flex xs4 class="pa-3">
                      <div>
                          <i class="fas fa-database icon"></i>
                          Daten
                      </div>
                      <br>
                      <div style="font-size:smaller">
                          <ul>
                            <li>
                              Statistisches Bundesamt. (2019).
                              <span style="font-style: italic;">
                                  BAföG - Geförderte Personen und finanzieller Aufwand: Bundesländer, Jahre, Personenkreis (21411-0002).
                              </span>
                              Abgerufen unter:
                              <a href="https://www-genesis.destatis.de/genesis/online/data?operation=find&suchanweisung_language=de&query=+21411-0002"> https://www-genesis.destatis.de/genesis/online/data?operation=find&suchanweisung_language=de&query=+21411-0002</a>.
                          </li>
                        </ul>
                        <br><br>
                        <span>
                            <i class="fab fa-github icon"></i>
                            <a href="https://github.com/clarissastaudt/Datenjournalismus_BAfoeG">Code</a>
                        </span>
                  </div>
                </v-flex>
              </v-row>
            </v-sheet>
        </v-bottom-sheet>
    </div>
</template>

<style>
.box-statistic {
    border: 1px solid #b3e5fc; padding: 15px 20px 20px;
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
    import countTo from 'vue-count-to'


    export default {
      name: 'Money',
      components: { countTo },

      data: () => ({
         sheet_money: false,
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
