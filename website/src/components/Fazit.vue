<template>
    <div style="text-align:left">
        <h1 class="font-weight-light light-blue--text" style="text-align:center">Wie geht es mit dem BAföG weiter?</h1>
        <br>
        <div style="text-align: justify">
                        <p>
                            Die letz&shy;te gro&shy;ße BAföG-Re&shy;form wur&shy;de im Jahr 2019 durch&shy;ge&shy;führt.
                            Es wur&shy;den die BAföG-Sätze, die Ein&shy;kommens&shy;frei&shy;beträge, die Wohn&shy;zu&shy;schläge, die Kranken- bzw. Pflege&shy;versicherungs&shy;zuschüsse und Zus&shy;chüsse für Stu&shy;dierende mit Kin&shy;dern an&shy;geho&shy;ben.
                            Zu&shy;dem sol&shy;len Ver&shy;schuldungs&shy;ängs&shy;te da&shy;durch re&shy;duz&shy;iert wer&shy;den, dass Stu&shy;die&shy;rende nach ihrem Ab&shy;schluss maximal 20 Jahre lang zur Til&shy;gung ihres BAföG-Dar&shy;lehens bei&shy;tra&shy;gen müs&shy;sen.
                            Neu ist auch, dass Stu&shy;dierende an nicht-staat&shy;lichen Berufs&shy;aka&shy;demien ge&shy;för&shy;dert wer&shy;den.
                        </p>
                        <p>
                            Ins&shy;gesamt bleibt zu hof&shy;fen, dass durch die&shy;se Maß&shy;nah&shy;men wie&shy;der mehr Stu&shy;die&shy;rende von einer För&shy;derung durch das BAföG pro&shy;fitie&shy;ren.
                            Die Le&shy;bens&shy;hal&shy;tungs&shy;kos&shy;ten unter&shy;schei&shy;den sich je nach Stadt in Deutsch&shy;land stark.
                            Die Be&shy;darfs&shy;sätze sind aber bundes&shy;weit ein&shy;heit&shy;lich.
                            Selbst wenn also die ak&shy;tuel&shy;le Er&shy;höhung für mehr Geld sorgt, bleiben noch strukturelle Ungerechtigkeiten erhalten.
                            <br>BAföG sollte neu gedacht und grundlegend reformiert werden.
                        </p>
      </div>

      <div style="text-align: center !important">
          <v-bottom-sheet v-model="sheet_fazit">
            <template v-slot:activator="{ on }">
              <v-btn
                v-on="on"
              >
                <i class="fas fa-plus" style="margin-right: 10px"></i>Quellen
              </v-btn>
            </template>

            <v-sheet class="pa-10">
                <i class="fas fa-times fa-lg float-right" @click="sheet_fazit = !sheet_fazit"></i>
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
                                     Bundesministerium für Bildung und Forschung (2019).
                                     <span style="font-style: italic;">
                                         Neuerungen zum 1. August 2019.
                                      </span>
                                      Abgerufen unter:
                                     <a href="https://www.xn--bafg-7qa.de/de/bafoeg-reform-das-aendert-sich-622.php">
                                         https://www.xn--bafg-7qa.de/&#8203;de/&#8203;bafoeg-reform-das-aendert-sich-622.php
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
        <br>

    </div>
</template>

<script>

export default {
  name: 'Info',

  data: () => ({
      sheet_fazit: false,
  }),
  methods: {
  },
  mounted: () => {
  }
};
</script>
