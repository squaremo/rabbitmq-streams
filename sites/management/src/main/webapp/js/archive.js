$(document).ready(function(){
  $("#startDate").datepicker({ dateFormat: 'dd/mm/yy' });
  $("#endDate").datepicker({ dateFormat: 'dd/mm/yy' });
  $('#startTime').timepickr({ rangeMin: ["00", "05", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55"]});
  $('#endTime').timepickr( {rangeMin: ["00", "05", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55"]});
});


