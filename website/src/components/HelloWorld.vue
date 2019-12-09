<template>
      <v-container>
        <v-layout
          text-center
          wrap
        >

              <v-flex
                xs12
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

                <h1 class="font-weight-light light-blue--text" style="text-align:left">Wie hat sich der Anteil der BAföG-Empfänger*innen unter den Studierenden entwickelt?</h1>

                <v-container
                    id="scroll-target"
                    style="padding: 0"
                >
                <br>
                <h3 style="margin-bottom: -100px; text-align:left; width: 93%; position: relative; left: 7%"  v-scroll="onScrollVerlauf">Deutschlandweite Entwicklung des Anteils der BAföG-Empfänger*innen unter den Studierenden</h3>
                <div style="font-weight: bold; text-align: left; position: relative; top: 270px;  transform-origin: 0 0; transform: rotate(-90deg); font-size: 15px;">Anteil der <br>BAföG-Empfänger</div>
                <div style="width: 93%; position: relative; left: 7%">
                    <div>
                        <div id="perc1">
                            <div class="perc">30%</div>
                            <hr class="descr">
                        </div>
                        <div id="perc2">
                            <div class="perc">25%</div>
                            <hr class="descr">
                        </div>
                        <div id="perc3">
                            <div class="perc">20%</div>
                            <hr class="descr">
                        </div>
                    </div>
                     <v-sparkline v-if="verlauf"
                        :value="percOverYears.value"
                        :gradient="percOverYears.gradient"
                        :smooth="percOverYears.radius || false"
                        :padding="percOverYears.padding"
                        :line-width="percOverYears.width"
                        :stroke-linecap="percOverYears.lineCap"
                        :gradient-direction="gradientDirection"
                        :fill="percOverYears.fill"
                        :type="percOverYears.type"
                        :auto-line-width="percOverYears.autoLineWidth"
                        auto-draw
                        auto-draw-duration=3000
                        height=50
                    ></v-sparkline>
                    <v-slider
                        v-model="slider.value"
                        step="5"
                        ticks="always"
                        tick-size="5"
                        :tick-labels="slider.ticksLabels"
                        class="pl-5 pr-5"
                    ></v-slider>

                    <div style="font-weight: bold;  font-size: 15px; position: relative; top: -5px;">Jahr</div>
                </div>



              <v-row align="center" justify="center">
                <v-flex xs9 md5>
                  <v-img
                    v-if="slider.value==0"
                    src="../assets/1998.png"
                    contain
                    style="width: 100%;"
                  ></v-img>
                  <v-img
                    v-if="slider.value==5"
                    src="../assets/1999.png"
                    contain
                    style="height: 100%;"
                  ></v-img>
                  <v-img
                    v-if="slider.value==10"
                    src="../assets/2000.png"
                    contain
                    style="height: 100%;"
                  ></v-img>
                  <v-img
                    v-if="slider.value==15"
                    src="../assets/2001.png"
                    contain
                    style="height: 100%;"
                  ></v-img>
                  <v-img
                    v-if="slider.value==20"
                    src="../assets/2002.png"
                    contain
                    style="height: 100%;"
                  ></v-img>
                  <v-img
                    v-if="slider.value==25"
                    src="../assets/2003.png"
                    contain
                    style="height: 100%;"
                  ></v-img>
                  <v-img
                    v-if="slider.value==30"
                    src="../assets/2004.png"
                    contain
                    style="height: 100%;"
                  ></v-img>
                  <v-img
                    v-if="slider.value==35"
                    src="../assets/2005.png"
                    contain
                    style="height: 100%;"
                  ></v-img>
                  <v-img
                    v-if="slider.value==40"
                    src="../assets/2006.png"
                    contain
                    style="height: 100%;"
                  ></v-img>
                  <v-img
                    v-if="slider.value==45"
                    src="../assets/2007.png"
                    contain
                    style="height: 100%;"
                  ></v-img>
                  <v-img
                    v-if="slider.value==50"
                    src="../assets/2008.png"
                    contain
                    style="height: 100%;"
                  ></v-img>
                  <v-img
                    v-if="slider.value==55"
                    src="../assets/2009.png"
                    contain
                    style="height: 100%;"
                  ></v-img>
                  <v-img
                    v-if="slider.value==60"
                    src="../assets/2010.png"
                    contain
                    style="height: 100%;"
                  ></v-img>
                  <v-img
                    v-if="slider.value==65"
                    src="../assets/2011.png"
                    contain
                    style="height: 100%;"
                  ></v-img>
                  <v-img
                    v-if="slider.value==70"
                    src="../assets/2012.png"
                    contain
                    style="height: 100%;"
                  ></v-img>
                  <v-img
                    v-if="slider.value==75"
                    src="../assets/2013.png"
                    contain
                    style="height: 100%;"
                  ></v-img>
                  <v-img
                    v-if="slider.value==80"
                    src="../assets/2014.png"
                    contain
                    style="height: 100%;"
                  ></v-img>
                  <v-img
                    v-if="slider.value==85"
                    src="../assets/2015.png"
                    contain
                    style="height: 100%;"
                  ></v-img>
                  <v-img
                    v-if="slider.value==90"
                    src="../assets/2016.png"
                    contain
                    style="height: 100%;"
                  ></v-img>
                  <v-img
                    v-if="slider.value==95"
                    src="../assets/2017.png"
                    contain
                    style="height: 100%;"
                  ></v-img>
                  <v-img
                    v-if="slider.value==100"
                    src="../assets/2018.png"
                    contain
                    style="height: 100%;"
                  ></v-img>
                </v-flex>
                <v-flex xs3 md2 class="legend">
                    <div>
                        <div class=legend-color style="background-color: #E1F5FE;"></div>
                        größer 15% bis 20%
                    </div>
                    <div>
                        <div class=legend-color style="background-color: #81D4FA;"></div>
                        größer 20% bis 25%
                    </div>
                    <div>
                        <div class=legend-color style="background-color: #29B6F6;"></div>
                        größer 25% bis 30%
                    </div>
                    <div>
                        <div class=legend-color style="background-color: #0288D1;"></div>
                        größer 30% bis 35%
                    </div>
                    <div>
                        <div class=legend-color style="background-color: #0266b5;"></div>
                        größer 35% bis 40%
                    </div>
                    <div>
                        <div class=legend-color style="background-color: #014880;"></div>
                        größer 40% bis 45%
                    </div>
                    <div>
                        <div class=legend-color style="background-color: #00335c;"></div>
                        größer 45% bis 50%
                    </div>
                </v-flex>
                <v-flex xs12 md5 style="text-align: left">
                   <p class="pa-5" v-if="slider.value == 0">
                    1998 sind Dinge passiert, die hier erklärt werden. Lorem ipsum
                   </p>
                   <p class="pa-5" v-if="slider.value == 5">
                    1999 sind Dinge passiert, die hier erklärt werden. Lorem ipsum
                   </p>
                   <p class="pa-5" v-if="slider.value == 10">
                    2000 sind Dinge passiert, die hier erklärt werden. Lorem ipsum
                   </p>
                   <p class="pa-5" v-if="slider.value == 15">
                    2001 sind Dinge passiert, die hier erklärt werden. Wiki: Nach der Bundestagswahl 1998 bildete Gerhard Schröder die erste rot-grüne Regierungskoalition. Sie beschloss 2001 eine weitreichende Reform, die viele Einschränkungen der Kohl-Ära zurücknahm. Zusätzliche Veränderungen erfolgten etwa durch die Freistellung des Kindergeldes bei der Einkommensanrechnung. Zudem müssen seither insgesamt nur maximal 10.000 Euro des Darlehens zurückgezahlt werden. Nach dieser Reform gewann das BAföG wieder wesentlich an Bedeutung. Im Jahr 2003 erhielten bereits mehr als 25 % der Studenten Förderung nach dem BAföG.
                   </p>
                   <p class="pa-5" v-if="slider.value == 20">
                    2002 sind Dinge passiert, die hier erklärt werden. Lorem ipsum
                   </p>
                   <p class="pa-5" v-if="slider.value == 25">
                    2003 sind Dinge passiert, die hier erklärt werden. Lorem ipsum
                   </p>
                   <p class="pa-5" v-if="slider.value == 30">
                    2004 sind Dinge passiert, die hier erklärt werden. Lorem ipsum
                   </p>
                   <p class="pa-5" v-if="slider.value == 35">
                    2005 sind Dinge passiert, die hier erklärt werden. Lorem ipsum
                   </p>
                   <p class="pa-5" v-if="slider.value == 40">
                    2006 sind Dinge passiert, die hier erklärt werden. Lorem ipsum
                   </p>
                   <p class="pa-5" v-if="slider.value == 45">
                    2007 sind Dinge passiert, die hier erklärt werden. Lorem ipsum
                   </p>
                   <p class="pa-5" v-if="slider.value == 50">
                    2008 sind Dinge passiert, die hier erklärt werden. Lorem ipsum
                   </p>
                   <p class="pa-5" v-if="slider.value == 55">
                    2009 sind Dinge passiert, die hier erklärt werden. Lorem ipsum
                   </p>
                   <p class="pa-5" v-if="slider.value == 60">
                    2010 sind Dinge passiert, die hier erklärt werden. Wiki: 2010 beschloss die Bundesregierung Merkel eine Novelle des BAföG. Die allgemeine Altersgrenze von 30 Jahren wurde für Masterstudiengänge auf 35 Jahre angehoben. Begabungs- und leistungsabhängige Stipendien bis zu 300 Euro monatlich werden seither von einer Anrechnung auf den Bedarfssatz nach BAföG ausgenommen. Die für die Ehe und für Ehegatten geltenden Regelungen im BAföG gelten nun auch für Partner einer eingetragenen Lebenspartnerschaft.[
                   </p>
                   <p class="pa-5" v-if="slider.value == 65">
                    2011 sind Dinge passiert, die hier erklärt werden. Lorem ipsum
                   </p>
                   <p class="pa-5" v-if="slider.value == 70">
                    2012 sind Dinge passiert, die hier erklärt werden. Wiki: Bis zum Jahr 2012 wurde ein Teilerlass für das zinslose staatliche Darlehen gewährt, wenn die Abschlussprüfung besonders gut oder die Ausbildung besonders schnell abgeschlossen wurde.
                   </p>
                   <p class="pa-5" v-if="slider.value == 75">
                    2013 sind Dinge passiert, die hier erklärt werden. Lorem ipsum
                   </p>
                   <p class="pa-5" v-if="slider.value == 80">
                    2014 sind Dinge passiert, die hier erklärt werden. Wiki: Mit dem 25. BAföG-Änderungsgesetz stimmte der Bundesrat am 19. Dezember 2014 der vollständigen Finanzierung der Ausbildungsförderung durch den Bund zu. Damit werden die Länder um jährlich rund 1,2 Milliarden Euro entlastet, die für zusätzliche Investitionen in die Hochschulen verwendet werden solle
                   <p class="pa-5" v-if="slider.value == 85">
                    2015 sind Dinge passiert, die hier erklärt werden. Lorem ipsum
                   </p>
                   <p class="pa-5" v-if="slider.value == 90">
                    2016 sind Dinge passiert, die hier erklärt werden. Wiki: Im August 2014 gab Bundesbildungsministerin Wanka bekannt, dass die Ausbildungsförderung ab Herbst 2016 um insgesamt 7 % angehoben wird. In diesem soll der Wohnzuschlag-Höchstsatz um 11 % angehoben werden. Nach amtlicher Berechnung soll der Anstieg der Verbraucherpreise seit der letzten Erhöhung 2010 bis 2014 bereits 7 % betragen. Zum Schuljahr bzw. Wintersemester 2016/17 hebt der Bund die Bedarfsätze und Freibeträge um 7 % an. Damit wird die Zahl der potentiellen Empfänger um rund 110.000 Studierende und Schüler erweitert. Drittstaatenangehörige mit Aufenthaltstiteln aus humanitären oder familiären Gründen können künftig bereits nach 15 Monaten statt bisher vier Jahren Aufenthalt in Deutschland BAföG beantragen.
                   </p>
                   <p class="pa-5" v-if="slider.value == 95">
                    2017 sind Dinge passiert, die hier erklärt werden. Lorem ipsum
                   </p>
                   <p class="pa-5" v-if="slider.value == 100">
                    2018 sind Dinge passiert, die hier erklärt werden. Lorem ipsum
                   </p>
                </v-flex>
              </v-row>
              </v-container>

              <v-expansion-panels>
                  <v-expansion-panel>
                    <v-expansion-panel-header>Daten</v-expansion-panel-header>
                    <v-expansion-panel-content>
                      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                    </v-expansion-panel-content>
                  </v-expansion-panel>
                  <v-expansion-panel>
                    <v-expansion-panel-header>Methode</v-expansion-panel-header>
                    <v-expansion-panel-content>
                      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                    </v-expansion-panel-content>
                  </v-expansion-panel>
               </v-expansion-panels>

              <br><br>
              <h1 class="font-weight-light light-blue--text" style="text-align:left">Warum gibt es Unterschiede zwischen den Bundesländern?</h1>

              <v-expansion-panels>
                  <v-expansion-panel>
                    <v-expansion-panel-header>Daten</v-expansion-panel-header>
                    <v-expansion-panel-content>
                      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                    </v-expansion-panel-content>
                  </v-expansion-panel>
                  <v-expansion-panel>
                    <v-expansion-panel-header>Methode</v-expansion-panel-header>
                    <v-expansion-panel-content>
                      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                    </v-expansion-panel-content>
                  </v-expansion-panel>
               </v-expansion-panels>

             </v-flex>


        </v-layout>
    </v-container>
</template>

<style type="text/css">
	#summary {
		font-size: 150%;
		text-align: left;
	}
	#authors span {
		font-weight: bold;
	}
	.descr {
		width: 95%;
		position: relative;
		left: 2.8%;
		border-top: 0.1px dotted #B3E5FC;
	}
	.perc {
		position: relative;
		top: 14px;
	}
    .legend {
        font-size: smaller;
    }
    .legend-color {
        position: relative;
        top: 7px;
        margin-right: 5px;
        height: 20px;
        width: 20px;
        border: 1px solid black;
        display: inline-block;
    }

	@media (max-width: 360px) {
		#perc1 {
			text-align: left; position: relative; top: 38px
		}
		#perc2{
			display: none
		}
		#perc3 {
			text-align: left; position: relative; top: 46px
		}
	}
	@media (min-width: 360px) {
		#perc1 {
			text-align: left; position: relative; top: 40px
		}
		#perc2{
			display: none
		}
		#perc3 {
			text-align: left; position: relative; top: 54px
		}
	}
	@media (min-width: 420px) {
		#perc1 {
			text-align: left; position: relative; top: 67px
		}
		#perc2{
			text-align: left; position: relative; top: 63px;
			display: block;
		}
		#perc3 {
			text-align: left; position: relative; top: 60px
		}
	}
	@media (min-width: 470px) {
		#perc1 {
			text-align: left; position: relative; top: 68px
		}
		#perc2{
			text-align: left; position: relative; top: 69px;
			display: block;
		}
		#perc3 {
			text-align: left; position: relative; top: 70px
		}
	}
	@media (min-width: 520px) {
		#perc1 {
			text-align: left; position: relative; top: 71px
		}
		#perc2{
			text-align: left; position: relative; top: 76px;
			display: block;
		}
		#perc3 {
			text-align: left; position: relative; top: 80px
		}
	}
	@media (min-width: 600px) {
		#perc1 {
			text-align: left; position: relative; top: 74px
		}
		#perc2{
			text-align: left; position: relative; top: 82px;
			display: block;
		}
		#perc3 {
			text-align: left; position: relative; top: 90px
		}
	}
	@media (min-width: 670px) {
		#perc1 {
			text-align: left; position: relative; top: 77px
		}
		#perc2{
			text-align: left; position: relative; top: 88px;
			display: block;
		}
		#perc3 {
			text-align: left; position: relative; top: 100px
		}
	}
	@media (min-width: 750px) {
		#perc1 {
			text-align: left; position: relative; top: 83px
		}
		#perc2{
			text-align: left; position: relative; top: 97px;
			display: block;
		}
		#perc3 {
			text-align: left; position: relative; top: 111px
		}
	}
	@media (min-width: 860px) {
		#perc1 {
			text-align: left; position: relative; top: 85px
		}
		#perc2{
			text-align: left; position: relative; top: 106px;
			display: block;
		}
		#perc3 {
			text-align: left; position: relative; top: 128px
		}
	}
	@media (min-width: 1264px) {
		#perc1 {
			text-align: left; position: relative; top: 74px
		}
		#perc2{
			text-align: left; position: relative; top: 85px;
			display: block;
		}
		#perc3 {
			text-align: left; position: relative; top: 97px
		}
	}
	@media (min-width: 1450px) {
		#perc1 {
			text-align: left; position: relative; top: 79px
		}
		#perc2{
			text-align: left; position: relative; top: 93px;
			display: block;
		}
		#perc3 {
			text-align: left; position: relative; top: 108px
		}
	}
	@media (min-width: 1600px) {
		#perc1 {
			text-align: left; position: relative; top: 80px
		}
		#perc2{
			text-align: left; position: relative; top: 98px;
			display: block;
		}
		#perc3 {
			text-align: left; position: relative; top: 117px
		}
	}
	@media (min-width: 1700px) {
		#perc1 {
			text-align: left; position: relative; top: 88px
		}
		#perc2{
			text-align: left; position: relative; top: 108px;
			display: block;
		}
		#perc3 {
			text-align: left; position: relative; top: 129px
		}
	}
	@media (min-width: 1850px) {
		#perc1 {
			text-align: left; position: relative; top: 85px
		}
		#perc2{
			text-align: left; position: relative; top: 110px;
			display: block;
		}
		#perc3 {
			text-align: left; position: relative; top: 135px
		}
	}
	@media (min-width: 2000px) {
		#perc1 {
			text-align: left; position: relative; top: 83px
		}
		#perc2{
			text-align: left; position: relative; top: 114px;
			display: block;
		}
		#perc3 {
			text-align: left; position: relative; top: 145px
		}
	}
	@media (min-width: 2200px) {
		#perc1 {
			text-align: left; position: relative; top: 90px
		}
		#perc2{
			text-align: left; position: relative; top: 125px;
			display: block;
		}
		#perc3 {
			text-align: left; position: relative; top: 160px
		}
	}
	@media (min-width: 2400px) {
		#perc1 {
			text-align: left; position: relative; top: 90px
		}
		#perc2{
			text-align: left; position: relative; top: 127px;
			display: block;
		}
		#perc3 {
			text-align: left; position: relative; top: 175px
		}
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

export default {
  name: 'HelloWorld',

  data: () => ({
    verlauf: false,
    slider: {
        value: 0,
        ticksLabels: yearsShort
    },
    percOverYears: {
      width: 1,
      radius: 1,
      padding: 8,
      lineCap: 'round',
      gradient: ['#0288D1','#81D4FA'],
      value: [0.2057649, 0.2121235, 0.2163986, 0.2447376, 0.2637627, 0.2715762, 0.2896462, 0.2917449, 0.2877431, 0.2895423, 0.2857571, 0.2933107, 0.3014509, 0.3041941, 0.3026595, 0.2875919, 0.2719744, 0.2528966, 0.2383740, 0.2252972, 0.2092836],
      gradientDirection: 'top',
      fill: false,
      type: 'trend',
      autoLineWidth: false,
      labels: yearsShort,
      labelSize: 3
    }
  }),
  methods: {
    onScrollVerlauf () {
        this.verlauf = true
    },
  },
  mounted: () => {
  }
};
</script>
