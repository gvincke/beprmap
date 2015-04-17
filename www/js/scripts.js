$(document).ready(function(){
    var adaptTownsSelectivity = function () {
    if ($("#selection").val()=="unselected")  {/*|| $("#selection").val()=="all"*/
        $('#towns').prop('disabled', false);
    }
    else {
        $('#towns').prop('disabled', 'disabled');
    }
  };
  $(adaptTownsSelectivity);
  $("#selection").change(adaptTownsSelectivity);

});

