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


  // Spinner när ny data laddas vid flikbyte
    var spinnerShownAt = 0;
    var spinnerMinTid  = 200;

    Shiny.addCustomMessageHandler('show-spinner', function(msg) {
      spinnerShownAt = Date.now();

      var n = (msg && msg.n) ? msg.n : 0;
      // Bastid + extra tid per segment. Justera de två talen efter dina egna mätningar.
      spinnerMinTid = 300 + n * 0.5;

      $('#app_spinner_overlay').addClass('show');
    });

    function doljSpinnerMedGolvtid() {
      var forfluten = Date.now() - spinnerShownAt;
      var vantaYtterligare = Math.max(0, spinnerMinTid - forfluten);
      setTimeout(function() {
        $('#app_spinner_overlay').removeClass('show');
      }, vantaYtterligare);
    }


