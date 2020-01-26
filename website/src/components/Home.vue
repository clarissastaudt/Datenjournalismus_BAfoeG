<template>

        <v-row>
            <div id="warning">
                Diese Seite wurde für kleine Bildschirmgrößen (noch) nicht optimiert, weshalb Anzeigefehler auftreten können. Falls möglich bitte die Fenstergröße erhöhen!
            </div>

            <v-flex
                xs12 sm6 offset-sm3 id="content-container"
              >
              <blockquote class="blockquote pa-0" id="summary">
                  <br>
                  <p>
                      Je&shy;de Per&shy;son, die ein Stu&shy;dium be&shy;ginnen möch&shy;te, muss sich im&shy;mer um eine Sache Ge&shy;dan&shy;ken ma&shy;chen:
                      „Wie be&shy;strei&shy;te ich meinen Lebens&shy;unter&shy;halt?“
                      Wer neben dem Stu&shy;dium ar&shy;beiten muss, hat we&shy;niger Zeit für das Stud&shy;ium zu ler&shy;nen.
                  </p>
                  <p>
                      Das Bundes&shy;aus&shy;bil&shy;dungs&shy;för&shy;derungs&shy;gesetz (BAföG) hat unter an&shy;derem das Ziel, dass bei nie&shy;mandem durch diese Fra&shy;ge ein Stu&shy;dium schei&shy;tert oder gar nicht erst auf&shy;ge&shy;nom&shy;men wird.
                      Dafür muss zum einen die För&shy;de&shy;rung bei Stu&shy;dierenden an&shy;kom&shy;men, die es brau&shy;chen.
                      Zum an&shy;deren muss die För&shy;derung aber auch rei&shy;chen, um den Le&shy;bens&shy;un&shy;ter&shy;halt zu ge&shy;währ&shy;leis&shy;ten.
                      Diese bei&shy;den Um&shy;stän&shy;de ent&shy;schei&shy;den, ob BAföG sinn&shy;voll wirkt, oder nicht.
                  </p>
                  <p>
                    Im Fol&shy;gen&shy;den wer&shy;fen wir einen Blick auf diese bei&shy;den Fak&shy;to&shy;ren und kom&shy;men zu ei&shy;nem eher er&shy;nüch&shy;tern&shy;den Er&shy;gebnis.
                  </p>
                  <br>
                  <p id="authors">
                      Von <span>Alina Burlakov</span>, <span>Clarissa Staudt</span>, <span>Marcus Mikorski</span>, <span>Nina Neser</span> und <span>Sun Mengqiong</span>
                  </p>
              </blockquote>

              <div style="height: 60px; max-height: 10vh"></div>

              <Development></Development>
              <br><br>
              <Info></Info>
              <br><br>
              <Money></Money>
              <br><br>
              <Fazit></Fazit>

             </v-flex>

             </v-row>

</template>

<style type="text/css">

    .v-dialog.v-dialog--active.v-bottom-sheet {
        overflow: scroll;
    }

    .blockquote {
        font-size: 20px !important;
    }

    #summary {
        text-align: justify;
        line-height: 1.3;
    }
    #authors span {
        font-weight: bold;
    }
    .icon {
        margin-right: 10px;
    }

    #content-container {
        max-width: 90vw;
        margin: 0 auto;
    }

    @media (max-width: 599px){
        #content-container {
        }
    }

    @media (min-width: 1050px) {
        #warning {
            display: none;
            visibility: hidden;
            height: 0px
        }
    }
    @media (max-width: 1050px) {
        #warning {
            position: fixed;
            bottom: 0;
            padding: 20px 10vw;
            z-index: 100;
            width: 102vw;
            text-align: center;
            background: #F44336;
        }
    }


</style>

<script>
import Development from './Development.vue'
import Money from './Money.vue'
import Info from './Info.vue'
import Fazit from './Fazit.vue'

export default {
  name: 'Home',
  components: {Development, Money, Info, Fazit},

  data: () => ({
  }),
  methods: {

  },
  mounted: () => {
  }
};
</script>
