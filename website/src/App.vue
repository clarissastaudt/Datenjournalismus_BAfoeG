<template>
  <v-app>
    <!--v-app-bar
      app
      color="primary"
      dark
    >
      <div class="d-flex align-center">
        BAföG
      </div>

      <v-spacer></v-spacer>


    </v-app-bar-->

    <v-layout>
    <v-flex xs12>
        <v-parallax src="../src/assets/uni5.jpg">

          <v-row
              align="center"
              justify="center"
          >

            <v-col class="text-center" cols="12" style="margin-left: 5vw; margin-top: -30vh">
              <h1 class="font-weight-thin light-blue--text" id="header">BAföG</h1>
               <h1 class="font-weight-thin black--text display-2 mt-n6">für alle?</h1>
            </v-col>

          </v-row>
        </v-parallax>
      </v-flex>
    </v-layout>

    <v-content>
      <v-layout>
        <v-flex col-lg-6 offset-lg-3>
          <HelloWorld/>
        </v-flex>
      </v-layout>
    </v-content>

      <v-footer
    dark
    padless
  >
    <v-card
      flat
      tile
      class="light-blue darken-3 white--text text-center"
    >

      <v-card-text class="white--text pt-0" style="width: 100vw">
        <br>
        <p>
          Dieses Projekt entstand im Wintersemester 2019/20 im Rahmen des Mastermoduls "Daten - IT - Kommunikation" am Karlsruher Institut für Technologie.
        </p>

        <div>
          <div>

            <p>Nina Neser <br>Sun Mengqiong <br>Alina Burlakov <br>Marcus Mikorski <br>Clarissa Staudt</p>
          </div>
        </div>

      </v-card-text>
      <v-divider></v-divider>
      <v-card-text>
        &copy; {{ new Date().getFullYear() }} - Projektgruppe BAföG
      </v-card-text>

    </v-card>
  </v-footer>

  </v-app>
</template>

<script>
import HelloWorld from './components/HelloWorld';

export default {
  name: 'App',

  components: {
    HelloWorld,
  },

  data: () => ({
    //
  }),
  mounted: function() {
    document.querySelector(".v-parallax").style.height = '66vh'
  }
};
</script>
<style>
    #header {
      font-size: 10em
    }
</style>
