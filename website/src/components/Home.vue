<template>
      <v-row>
        <v-layout
          text-center
          wrap
        >
            <v-flex
                xs6 offset-xs3
              >
              <blockquote class="blockquote pa-0" id="summary">
                  <br>
                  <p>
                      Lorem ipsum dolor sit amet, consectetur adipisicing elit. Harum maiores modi quidem veniam, expedita quis laboriosam, ullam facere adipisci, iusto, voluptate sapiente corrupti asperiores rem nemo numquam fuga ab at.
                  </p>
                  <br>
                  <p id="authors">
                      Von <span>Nina Neser</span>, <span>Sun Mengqiong</span>, <span>Alina Burlakov</span>, <span>Marcus Mikorski</span> und <span>Clarissa Staudt</span>
                  </p>
              </blockquote>

              <div style="height: 60px; max-height: 10vh"></div>

              <Development></Development>
              <br><br>
              <ModelBL></ModelBL>
              <br><br>
              <Money></Money>

             </v-flex>


        </v-layout>
    </v-row>
</template>

<style type="text/css">

    #summary {
        font-size: 150%;
        text-align: left;
    }
    #authors span {
        font-weight: bold;
    }
    .icon {
        margin-right: 10px;
    }




</style>

<script>
import Development from './Development.vue'
import Money from './Money.vue'
import ModelBL from './ModelBL.vue'

export default {
  name: 'Home',
  components: {Development, Money, ModelBL},

  data: () => ({
  }),
  methods: {

  },
  mounted: () => {
  }
};
</script>
