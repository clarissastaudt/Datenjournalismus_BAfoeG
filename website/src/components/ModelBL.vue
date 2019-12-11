<template>
    <div>
        <h1 class="font-weight-light light-blue--text" style="text-align:left">Warum bekommen Studierende im Osten häufiger BAföG?</h1>
        <br>
        <div>
            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
      </div>
        <br>
        <v-bottom-sheet v-model="sheet_lm">
          <template v-slot:activator="{ on }">
            <v-btn
              v-on="on"
            >
              <i class="fas fa-plus" style="margin-right: 10px"></i>Methodik
            </v-btn>
          </template>

          <v-sheet class="pa-10">


              <i class="fas fa-times fa-lg float-right" @click="sheet_lm= !sheet_lm"></i>


            <v-row align="left" justify="left">
                <v-flex xs4 class="pa-3">
                    <div>
                        <i class="fas fa-list-ol icon"></i>
                        Vorgehen
                    </div>
                    <br>
                    <div style="font-size:smaller">
                        Multiple lineare Regression
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
                                  Studierende: Bundesländer, Semester, Nationalität, Geschlecht (21311-0005).
                              </span>
                              Abgerufen unter:
                              <a href="https://www-genesis.destatis.de/genesis/online/data?operation=find&suchanweisung_language=de&query=21311-0005"> https://www-genesis.destatis.de/genesis/online/data?operation=find&suchanweisung_language=de&query=21311-0005</a>.
                          </li>
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

<script>

export default {
  name: 'ModelBL',

  data: () => ({
     sheet_lm: false,
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
            this.$refs.countkosten.start();
            this.$refs.countbafog.start();
            this.$refs.countbafogaverage.start();
        }
  },
  mounted: () => {
  }
};
</script>
