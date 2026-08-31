  function rdUpdateMapLayout(value) {
    const noMap = value === "start" || value === "om";
    document.body.classList.toggle("utan-karta", noMap);
    // Leaflet behöver ofta sparkas lite när containern ändrar storlek
    setTimeout(function () {
      window.dispatchEvent(new Event("resize"));
      const widget = HTMLWidgets.find("#delad_karta");
      if (widget && widget.getMap) {
        widget.getMap().invalidateSize();
      }
    }, 150);
  }

  $(document).on("shiny:connected", function () {
    const navValue = Shiny.shinyapp.$inputValues.nav;
    rdUpdateMapLayout(navValue || "start");
  });

  $(document).on("shiny:inputchanged", function (event) {
    if (event.name === "nav") {
      rdUpdateMapLayout(event.value);
      visaSpinnerDirekt();
    }
  });

  // Maximera karta
  Shiny.addCustomMessageHandler("toggle-expand", function (message) {
    document.body.classList.toggle("karta-maximerad", message.expand);
    setTimeout(function () {
      window.dispatchEvent(new Event("resize"));
      const widget = HTMLWidgets.find("#delad_karta");
      if (widget && widget.getMap) {
        widget.getMap().invalidateSize();
      }
    }, 150);
  });

    // Spinner-hantering: händelsebaserad detektering + proportionell buffert EFTER bekräftad målning
    var currentSegmentCount = 0;

    Shiny.addCustomMessageHandler('show-spinner', function(msg) {
      currentSegmentCount = (msg && msg.n) ? msg.n : 0;
      $('#app_spinner_overlay').addClass('show');
    });

    function visaSpinnerDirekt() {
      $('#app_spinner_overlay').addClass('show');
    }

    function doljSpinnerEfterMalning() {
      // Fast bastid + proportionell buffert PER SEGMENT, applicerad EFTER att
      // vi redan bekräftat (via settle + dubbel rAF) att målningen är klar.
      // Detta är en tilläggsbuffert, inte en golvtid som kan "hinnas ikapp".
      var buffert = 150 + currentSegmentCount * 0.03;

      setTimeout(function() {
        $('#app_spinner_overlay').removeClass('show');
      }, buffert);
    }

    // ---- Rutt-lager: byggs manuellt från cachead GeoJSON, ersätter addPolylines() ----
    var rdRuttLayer = null;

    Shiny.addCustomMessageHandler('update-rutt-layer', function(msg) {
      var map = HTMLWidgets.find('#delad_karta').getMap();
      var geojson = JSON.parse(msg.geojson);

      if (rdRuttLayer) {
        map.removeLayer(rdRuttLayer);
      }

      rdRuttLayer = L.geoJSON(geojson, {
        style: function(feature) {
          return {
            color: feature.properties.color_val,
            weight: 3,
            opacity: 1
          };
        },
        onEachFeature: function(feature, layer) {
          layer.on('click', function(e) {
            Shiny.setInputValue('delad_karta_shape_click', {
              id: feature.properties.lank_id,
              lat: e.latlng.lat,
              lng: e.latlng.lng,
              '.nonce': Math.random()
            }, { priority: 'event' });
          });
        }
      }).addTo(map);

      doljSpinnerEfterMalning();   // samma spinnerlogik som innan, återanvänd rakt av
});
