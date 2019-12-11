import Vue from 'vue'
import App from './App.vue'
import vuetify from './plugins/vuetify';
import VueRouter from 'vue-router'

Vue.use(VueRouter)
Vue.config.productionTip = false

import Home from './components/Home.vue'
import Impressum from './components/Impressum.vue'

const router = new VueRouter({
  routes: [
    { path: '/', component: Home },
    { path: '/impressum', component: Impressum}
  ]
})


new Vue({
  vuetify,
  router,
  render: h => h(App)
}).$mount('#app')
