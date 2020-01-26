<template>
    <div style="text-align:left">
        <h1 class="font-weight-light light-blue--text" style="text-align:center">Was ist eigentlich BAföG?</h1>
        <br>
        <div style="text-align: justify">
            <p>
                BAföG ist eine staat&shy;liche finan&shy;zielle Unter&shy;stützung für jun&shy;ge Men&shy;schen in Deutsch&shy;land.
                Voll aus&shy;ge&shy;schrie&shy;ben steht BAföG für „Bundes&shy;aus&shy;bil&shy;dungs&shy;för&shy;derungs&shy;gesetz“.
                Mit dieser Unter&shy;stützung er&shy;möglicht der Staat jun&shy;gen Er&shy;wachsen&shy;en eine Aus&shy;bil&shy;dung nach ih&shy;ren Inter&shy;essen und Fähig&shy;keiten, un&shy;ab&shy;hän&shy;gig von ihrer finan&shy;ziellen oder so&shy;ziale Situ&shy;ation.
            </p>
            <p>
                Ge&shy;fördert wird die erste Aus&shy;bil&shy;dung an berufs&shy;bilden&shy;den Schu&shy;len, Kollegs, Aka&shy;demien und Hoch&shy;schulen.
                Deu&shy;tsche Staats&shy;ange&shy;hörige kön&shy;nen grund&shy;sätzlich BAföG em&shy;pfan&shy;gen.
                Bürger*innen der Euro&shy;pä&shy;ischen Union, Migrant*innen und Ge&shy;flüch&shy;tete kön&shy;nen nur dann BAföG er&shy;hal&shy;ten, wenn sie eine Bleibe&shy;perspek&shy;tive in Deutsch&shy;land ha&shy;ben und als ge&shy;sell&shy;schaftlich inte&shy;griert gel&shy;ten.
                Außer&shy;dem soll&shy;te die schu&shy;lische Aus&shy;bildung oder das Stu&shy;dium vor dem 30. Lebens&shy;jahr be&shy;gon&shy;nen wer&shy;den.
                Bei einem Master&shy;studium soll&shy;te der Beginn vor dem 35. Lebens&shy;jahr lie&shy;gen.
            </p>
            <p>
                Ob eine Per&shy;son förder&shy;fähig ist, über&shy;prüft das BAföG-Amt.
                Ein zen&shy;trales Kri&shy;te&shy;rium ist da&shy;bei, ob die Aus&shy;bil&shy;dung mit eige&shy;nen Mit&shy;teln oder durch die El&shy;tern, den Ehe&shy;gatten oder ein&shy;ge&shy;tra&shy;genen Lebens&shy;partner selbstän&shy;dig fi&shy;nan&shy;ziert wer&shy;den kann.
                Eine Teil&shy;förde&shy;rung er&shy;hal&shy;ten Studie&shy;rende, wenn die El&shy;tern oder Ehe&shy;gat&shy;te bzw. Lebens&shy;part&shy;ner Tei&shy;le der Aus&shy;bil&shy;dung selbstän&shy;dig über&shy;neh&shy;men kön&shy;nen.
                Rei&shy;chen die finan&shy;ziellen Mit&shy;tel der Fami&shy;lie nicht aus, wird eine Voll&shy;för&shy;de&shy;rung ge&shy;währt.
                Um BAföG zu be&shy;zie&shy;hen, muss ein Nach&shy;weis über eine Auf&shy;nahme an einer Schu&shy;le oder einer Hoch&shy;schule er&shy;bracht wer&shy;den.
                Ab dem 5. Se&shy;mester müs&shy;sen zu&shy;sätzlich Leis&shy;tungs&shy;nach&shy;weise an das BAföG-Amt ge&shy;schickt wer&shy;den.
                Fünf Jah&shy;re nach dem En&shy;de der För&shy;derungs&shy;höchst&shy;dauer muss mit der Rück&shy;zah&shy;lung der im Stu&shy;dium er&shy;hal&shy;tenen BAföG-För&shy;derung be&shy;gonnen wer&shy;den.
                Dabei wer&shy;den durch den Staat kei&shy;ne Zin&shy;sen be&shy;rechnet.
            </p>


      </div>

      <div style="text-align: center !important">
          <v-bottom-sheet v-model="sheet_info">
            <template v-slot:activator="{ on }">
              <v-btn
                v-on="on"
              >
                <i class="fas fa-plus" style="margin-right: 10px"></i>Quellen
              </v-btn>
            </template>

            <v-sheet class="pa-10">
                <i class="fas fa-times fa-lg float-right" @click="sheet_info = !sheet_info"></i>
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
                                         Das BAföG: alle Infos auf einen Blick. Fragen und Antworten.
                                      </span>
                                      Abgerufen unter:
                                     <a href="https://www.xn--bafg-7qa.de/de/welche-bedarfssaetze-sieht-das-bafoeg-vor--375.php">
                                         https://www.xn--bafg-7qa.de/&#8203;de/&#8203;welche-bedarfssaetze-sieht-das-bafoeg-vor--375.php
                                     </a>.
                                 </li>
                                 <li>
                                     Bundesministerium für Bildung und Forschung (2020).
                                     <span style="font-style: italic;">
                                         Darlehensrückzahlung.
                                      </span>
                                      Abgerufen unter:
                                     <a href="https://www.xn--bafg-7qa.de/de/darlehensrueckzahlung-200.php">
                                         https://www.xn--bafg-7qa.de/&#8203;de/&#8203;darlehensrueckzahlung-200.php
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
      sheet_info: false,
  }),
  methods: {
  },
  mounted: () => {
  }
};
</script>
