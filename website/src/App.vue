<template>
  <v-app>

    <v-layout>
         <v-row>
            <v-flex xs12>
                <v-parallax src="../src/assets/uni.jpg">

                  <v-row
                      align="center"
                      justify="center"
                  >

                    <v-col class="text-center" cols="6" style="background-color: rgba(3, 169, 244, 0.9)">
                      <h1 class="font-weight-thin white--text" id="header">BAföG</h1>
                       <h1 class="font-weight-thin white--text display-2 mt-n6">für alle?</h1>
                       <br>
                       <br>
                    </v-col>

                  </v-row>
                </v-parallax>
              </v-flex>
          </v-row>
      </v-layout>
      <v-layout>

              <router-view></router-view>
        
      </v-layout>


    <br>
    <br>
    <br>
    <br>
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
          Dieses Projekt entsteht zur Zeit im Rahmen des Mastermoduls "Daten - IT - Kommunikation" am Karlsruher Institut für Technologie.
        </p>

        <div>
          <div>

            <p>Nina Neser <br>Sun Mengqiong <br>Alina Burlakov <br>Marcus Mikorski <br>Clarissa Staudt</p>
          </div>
        </div>

      </v-card-text>
      <v-divider></v-divider>
      <v-card-text>
          <nav>
            <router-link to="/impressum" class="white--text">Impressum</router-link>
          </nav>
      </v-card-text>
      <v-card-text>
        &copy; {{ new Date().getFullYear() }} - Projektgruppe BAföG
      </v-card-text>

    </v-card>
  </v-footer>

  </v-app>
</template>

<script>

export default {
  name: 'App',

  components: {
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
