$(function() {
	console.log("CHARGEMENT DE DONNEES");
	$.ajax({
		url : '/solver',
		type : 'POST',
		data : {},
		success : function(reponse) {
;
			handleScheduleData(reponse);

		},
		error : function(er) {
			console.log(er);

		}
	});

});

function handleScheduleData(data) {
	var json = JSON.parse(data);
	var result = "";
	
	// Yes, We got a result
	if (json.hasOwnProperty("series")) {
		console.log("DONNEES RECUS");
		for (serie in json.series) {
			console.log("MA SERIE :" + serie);
			// First part : main header of the table

			result += '<div class="Timetable table-responsive">'
					+ '<table class="Timetable__table table table-bordered">'
					+ '<thead><tr><th colspan="' + (json.days.length + 1)
					+ '">Horaire IPL ' + serie + '</th></tr></thead><tbody>';

			// Second part : days header
			result += '<tr class="Timetable__time"><th class="Timetable__room"></th>';

			for (day in json.days) {
				result += '<th>' + json.days[day] + '</th>';
			}

			result += '</tr>';

			// Third part : courses data

			for (hour in json.hours) {
				result += '<tr><th class="Timetable__room">' + json.hours[hour] + '</th>';
				
				for (key in json.series[serie][json.hours[hour]]) {
					if ( ! json.series[serie][json.hours[hour]][key].hasOwnProperty("teacherName") ) {
						result += '<td></td>';
					} else {
						result += "<td> <div class=\"Allocation\">"
								+ "<span class=\"Allocation__title\">"
								+ json.series[serie][json.hours[hour]][key].courseName
								+ "</span><span class=\"Allocation__tutor\">"
								+ json.series[serie][json.hours[hour]][key].teacherName
								+ "</span>"
								+ "</span><span class=\"Allocation__tutor\">"
								+ "Local :"
								+ json.series[serie][json.hours[hour]][key].room
								+ "</span> </div> </td>";
					}
				}

				result += '</tr>';
			}

			result += '</tbody> </table> </div>';
		}

	} else {
		// no result for the reseach
		result += '<div class="alert alert-warning"><strong>Erreur !</strong> Pas de résultat</a>.</div>';
	}

	$(".tablesArea").append(result);
}

/*
 * EXAMPLE OF EXPECTED JSON : (use site like JSON beautiful to make it clear
 * (Eclipse doesn't like code indexation for JSON XD) { "days": [ "Monday",
 * "...." ], "hours": ["8H30", ".."], "series": { "Série 1": { "8H30": [{
 * "teacher": "Mister X", "room": "A017" }, { "notAcourse": true }], "10H30": [{
 * "teacher": "Mister X", "room": "A017" }, { "notAcourse": true }] } } }
 */