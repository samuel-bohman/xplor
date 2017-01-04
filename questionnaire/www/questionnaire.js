$(document).ready(function() {
	Shiny.addCustomMessageHandler("saveQuestionnaireHandler", saveQuestionnaireHandler);
	Shiny.addCustomMessageHandler("loadQuestionnaireHandler", loadQuestionnaireHandler);

	var ranking_text_type_a = [
	'är något bättre än',
	'är bättre än',
	'är markant bättre än',
	'är mycket bättre än',
	'är väldigt mycket bättre än',
	'är extremt mycket bättre än',
	'är extremt mycket bättre än (+)',
	'är extremt mycket bättre än (++)',
	'är extremt mycket bättre än (+++)',
	'är extremt mycket bättre än (++++)',
	'är extremt mycket bättre än (+++++)',
	'är extremt mycket bättre än (++++++)',
	'är extremt mycket bättre än (+++++++)',
	'är extremt mycket bättre än (++++++++)'
	];

	var ranking_text_type_b = [
	'är lika viktigt som',
	'är något viktigare än',
	'är viktigare än',
	'är markant viktigare än',
	'är markant viktigare än',
	'är extremt mycket viktigare än'
	];

	var ranking_text_type_c = [
	'är något viktigare än',
	'är viktigare än',
	'är markant viktigare än',
	'är mycket viktigare än',
	'är väldigt mycket viktigare än',
	'är extremt mycket viktigare än',
	'är extremt mycket viktigare än (+)',
	'är extremt mycket viktigare än (++)',
	'är extremt mycket viktigare än (+++)',
	'är extremt mycket viktigare än (++++)',
	'är extremt mycket viktigare än (+++++)',
	'är extremt mycket viktigare än (++++++)',
	'är extremt mycket viktigare än (+++++++)',
	'är extremt mycket viktigare än (++++++++)'
	];

	$(document).data('ranking-text-type-a', ranking_text_type_a);
	$(document).data('ranking-text-type-b', ranking_text_type_b);
	$(document).data('ranking-text-type-c', ranking_text_type_c);

	$(document).click(function(event) {
		if ($(event.target).closest("div[class*='section']").length == 0) {
			$('.focused').removeClass('focused');
		}
	});

	$(document).mouseup(function(event) {
		if ($(document).data('slider-mouse-pos')) {
			$(document).off('mousemove');
		}

		$(document).data('slider-mouse-pos', null);
	});
});

function changeRankedChoiceSliderBarSteps(event) {
	var value = parseInt($(event.target).val());

	$(event.target).closest("div[class='question-type-ranked-choice']").children("svg[class*='ranked-choice-slider-bar']").attr('data-steps', value);
}

function rankedChoiceOnMouseDown(event) {
	var slider_bar = $(event.target).closest('ul').siblings("svg[class*='ranked-choice-slider-bar']");
	var choice = $(event.target).closest('li');
	var slider_drag_square = choice.children("div[class*='ranked-choice-slider-drag-handle']");
	var choice_list = choice.closest('ul');
	var choice_position = parseInt(choice.attr('data-position'));
	var total_width = slider_bar.outerWidth() - $(slider_drag_square).outerWidth();
	var steps = parseInt(slider_bar.data('steps'));
	var dx = total_width / (steps - 1);
	var slider_drag_square_offset_x = event.pageX - $(slider_drag_square).offset().left;
	var order_description = choice_list.siblings("p[class*='ranked-choice-order-description']");
	var choice_type = slider_bar.attr('data-choice-type');

	slider_drag_square.addClass('is-dragging');
	event.preventDefault();

	$(document).on('mouseup', {slider_drag_square: slider_drag_square}, function(event) {
	$(document).off('mousemove');
	$(slider_drag_square).removeClass('is-dragging');

	}).on('mousemove', 
	{choice: choice,
	slider_drag_square: slider_drag_square,
	slider_drag_square_offset_x: slider_drag_square_offset_x},
	function(event) {
		var steps = parseInt(slider_bar.attr('data-steps'));
		var step_bound = Math.floor(steps/2);
		var choice_position = choice.attr('data-position');
		var relative_x_pos = event.pageX - (slider_drag_square.offset().left + slider_drag_square_offset_x);

		if (relative_x_pos > dx && choice_position < step_bound) {
			// move to the right
			moveRankedChoice('right', choice, steps, slider_bar);
			updateRankedChoiceOrderDescription(choice_type, order_description, choice_list);
		} else if (relative_x_pos < -dx && choice_position > -Math.floor(steps/2)) {
			// move to the left
			moveRankedChoice('left', choice, steps, slider_bar);
			updateRankedChoiceOrderDescription(choice_type, order_description, choice_list);
		}
	});
}

function moveRankedChoice(direction, choice, steps, slider_bar) {
	var old_position = parseInt($(choice).attr('data-position'));
	var new_position = old_position;

	if (direction == 'right') {		
		new_position++;
	} else if (direction == 'left') {
		new_position--;
	}

	var offset = (new_position/(steps-1)) * 100;

	$(choice).attr('data-position', new_position);
	$(choice).css('left', offset.toFixed(2) + '%');

	// Update slider bar gradient
	updateRankedChoiceSliderBarGradient(choice, slider_bar);
}

function updateRankedChoiceSliderBarGradient(choice, slider_bar) {
	var min = 1000;
	var max = -1000;
	var steps = parseInt($(slider_bar).attr('data-steps'));
	var choice_type = slider_bar.attr('data-choice-type');

	$(choice).closest('ul').children('li').each(function(index, element) {
		var position = parseInt($(element).attr('data-position'));

		if (position >= max) {
			max = position;
		}

		if (position <= min) {
			min = position;
		}
	});

	max += Math.floor(steps/2);
	min += Math.floor(steps/2);

	min = (min*100)/(steps-1);
	max = (max*100)/(steps-1);

	if (choice_type == 'A') {
		$(slider_bar).find(".ranked-choice-type-a-red-gradient-stop").attr('offset', Math.min(Math.round(min), 50) + '%');	
		$(slider_bar).find(".ranked-choice-type-a-green-gradient-stop").attr('offset', Math.max(Math.round(max), 50) + '%');
	} else if (choice_type == 'B') {
		$(slider_bar).find(".ranked-choice-type-b-blue-1-gradient-stop").attr('offset', Math.min(Math.round(min), 50) + '%');
		$(slider_bar).find(".ranked-choice-type-b-blue-2-gradient-stop").attr('offset', Math.max(Math.round(max), 50) + '%');
	} else if (choice_type == 'C') {
		$(slider_bar).find(".ranked-choice-type-c-blue-gradient-stop").attr('offset', Math.round(max) + '%');
	}
}

function toggleSliderBarMiddleLine(element) {
	var svg = $(element).parent().siblings("svg[class*='ranked-choice-slider-bar']");

	if ($(element).is(':checked')) {
		svg.children('line').attr('stroke-width', '1');
	} else {
		svg.children('line').attr('stroke-width', '0');
	}
}

function setRankedChoiceOrderDescription(choice_type, order_description, ranked_choice_list) {
	if (choice_type == 'A' || choice_type == 'C') {
		var str = 'Alternativ ';

		for (i = 1; i < ranked_choice_list.children('li').length; i++) {
			if (i == 1) {
				str += i;
			} else {
				str += (', ' + i);
			}
		}

		order_description.html(str + " <span style=\"font-weight: bold\">är lika bra</span>");

	} else if (choice_type == 'B') {
		order_description.html("Alternativ A <span style=\"font-weight: bold\">är lika viktigt som</span> alternativ B");
	}
}

function changeRankedChoiceType(element) {
	var ranked_choice_scale_list = $(element).parent().siblings("ul[class*='ranked-choice-scale-list']");
	var ranked_choice_list = $(element).closest("div[class*='question-type-ranked-choice']").children('ul.ranked-choice-list');
	var ranked_choice_slider_bar = ranked_choice_list.siblings("svg[class*='ranked-choice-slider-bar']");
	var ranked_choice_middle_line_checkbox = ranked_choice_list.closest("div[class*='question-type-ranked-choice']").find("input[class*='ranked-choice-show-middle-line-checkbox']");		
	var ranked_choice_order_description = ranked_choice_list.siblings("p[class*='ranked-choice-order-description']");
	var arrow_item = ranked_choice_list.children('li:first-child');
	var arrow_item_slider_drag_square = $(arrow_item).find("div[class*='ranked-choice-slider-drag-handle']");
	var arrow_item_polygon = $(arrow_item).find("svg[class*='ranked-choice-polygon']");
	var arrow_item_text = $(arrow_item).find("span[class*='ranked-choice-text']");
	var ranked_choice_number_of_steps = ranked_choice_list.closest("div[class*='question-type-ranked-choice']").find("input[class*='ranked-choice-number-of-steps']");
	var steps = ranked_choice_slider_bar.data('steps');

	if ($(element).val() == 'A') {
		ranked_choice_slider_bar.children('rect').attr('fill', 'url(#grad1)');

		ranked_choice_scale_list.children('li:nth-child(1)').children('span').text("Dåligt");
		ranked_choice_scale_list.children('li:nth-child(2)').children('span').text("Varken eller");
		ranked_choice_scale_list.children('li:nth-child(3)').children('span').text("Bra");

		ranked_choice_list.children('li').css('left', '0').attr('data-position', 0);
		ranked_choice_slider_bar.attr('data-choice-type', 'A');
		ranked_choice_list.siblings("button[class*='btn-add-alternative']").prop('disabled', false);

		ranked_choice_list.children('li').css('display', 'block');
		ranked_choice_slider_bar.children('line').attr('stroke-width', '1');
		ranked_choice_middle_line_checkbox.prop('checked', true);
		ranked_choice_number_of_steps.val('15');
		ranked_choice_slider_bar.attr('data-steps', '15');

		$(ranked_choice_slider_bar).find(".ranked-choice-type-a-red-gradient-stop").attr('offset', '50%');	
		$(ranked_choice_slider_bar).find(".ranked-choice-type-a-green-gradient-stop").attr('offset', '50%');

		setRankedChoiceOrderDescription('A', ranked_choice_order_description, ranked_choice_list);
		
	} else if ($(element).val() == 'B') {
		ranked_choice_slider_bar.children('rect').attr('fill', 'url(#grad2)');

		ranked_choice_scale_list.children('li:nth-child(1)').children('span').text("Alternativ A");
		ranked_choice_scale_list.children('li:nth-child(2)').children('span').text("Lika viktiga");
		ranked_choice_scale_list.children('li:nth-child(3)').children('span').text("Alternativ B");

		ranked_choice_list.children('li').css('left', '0').attr('data-position', 0);
		ranked_choice_slider_bar.attr('data-choice-type', 'B');
		ranked_choice_list.siblings("button[class*='btn-add-alternative']").prop('disabled', true);

		ranked_choice_list.children('li').css('display', 'none');

		ranked_choice_slider_bar.children('line').attr('stroke-width', '1');
		ranked_choice_middle_line_checkbox.prop('checked', true);
		ranked_choice_number_of_steps.val('11');
		ranked_choice_slider_bar.attr('data-steps', '11');

		$(ranked_choice_slider_bar).find(".ranked-choice-type-b-blue-1-gradient-stop").attr('offset', '50%');
		$(ranked_choice_slider_bar).find(".ranked-choice-type-b-blue-2-gradient-stop").attr('offset', '50%');

		arrow_item.css('display', 'block');

		arrow_item_slider_drag_square.css('margin-bottom', (-ranked_choice_list.outerHeight() + $(arrow_item).outerHeight()) + 'px');

		arrow_item_polygon.css('height', ranked_choice_list.outerHeight() + $(arrow_item).outerHeight() - $(arrow_item).outerHeight() + 'px');

		setRankedChoiceOrderDescription('B', ranked_choice_order_description, ranked_choice_list);

	} else if ($(element).val() == 'C') {
		ranked_choice_slider_bar.children('rect').attr('fill', 'url(#grad3)');

		ranked_choice_scale_list.children('li:nth-child(1)').children('span').text('');
		ranked_choice_scale_list.children('li:nth-child(2)').children('span').text('');
		ranked_choice_scale_list.children('li:nth-child(3)').children('span').text('');

		ranked_choice_list.children('li').css('left', '-50%').attr('data-position', -Math.floor(steps/2));
		ranked_choice_slider_bar.attr('data-choice-type', 'C');
		ranked_choice_list.siblings("button[class*='btn-add-alternative']").prop('disabled', false);

		ranked_choice_list.children('li').css('display', 'block');
		ranked_choice_slider_bar.children('line').attr('stroke-width', '0');
		ranked_choice_middle_line_checkbox.prop('checked', false);
		ranked_choice_number_of_steps.val('15');
		ranked_choice_slider_bar.attr('data-steps', '15');

		arrow_item.css('display', 'none');

		$(ranked_choice_slider_bar).find(".ranked-choice-type-c-blue-gradient-stop").attr('offset', '0%');

		setRankedChoiceOrderDescription('C', ranked_choice_order_description, ranked_choice_list);
	}
}

function addRankedChoice(element) {
	var choice_list = $(element).siblings('ul.ranked-choice-list');
	var choice_type = $(choice_list).siblings("svg[class*='ranked-choice-slider-bar']").attr('data-choice-type');
	var order_description = $(choice_list).siblings("p[class*='ranked-choice-order-description']");

	if (choice_list.children().length == 2) {
		choice_list.children('li:nth-child(2)').find("a[class*='ranked-choice-remove']").css('display', 'initial');
	}

	var cloned_choice = choice_list.children('li:last-child').clone();
	var cloned_choice_text = cloned_choice.find("span[class*='ranked-choice-text']");
	var cloned_choice_slider_drag_square = cloned_choice.find("div[class*='ranked-choice-slider-drag-handle']");
	var cloned_ranked_choice_polygon = cloned_choice.find("svg[class*='ranked-choice-polygon']");

	cloned_choice_text.text('Alternativ ' + (choice_list.children().length));

	choice_list.children('li').each(function(index, element) {
		$(element).find("div[class*='ranked-choice-slider-drag-handle']").css('margin-bottom', (-choice_list.outerHeight() + $(element).position().top) + 'px');
		$(element).find("svg[class*='ranked-choice-polygon']").css('height', choice_list.outerHeight() + $(element).outerHeight() - $(element).position().top + 'px');
	});

	choice_list.append(cloned_choice);

	updateRankedChoiceOrderDescription(choice_type, order_description, choice_list)
}

function updateRankedChoiceOrderDescription(choice_type, order_description, choice_list) {
	if (choice_type == 'A' || choice_type == 'C') {
		var choice_arr = new Array();
		var first_choice_ranking = parseInt($(choice_list).children('li:nth-child(2)').attr('data-position'));
		var all_choices_same_ranking = true;
		var order_description_string = '';

		$(choice_list).children('li:not(:first-child)').each(function(index, element) {
			var choice_ranking = parseInt($(element).attr('data-position'));

			if (choice_ranking !== first_choice_ranking) {
				all_choices_same_ranking = false;
			}

			choice_arr.push([$(element).children("span[class*='ranked-choice-text']").text(), parseInt($(element).attr('data-position'))]);
		});

		if (all_choices_same_ranking == true) {
			for (i = 0; i < choice_arr.length; i++) {
				if (i == 0) {
					order_description_string += choice_arr[i][0];
				} else {
					order_description_string += (', ' + choice_arr[i][0]);
				}
			}

			order_description_string += " <span style=\"font-weight: bold\">är lika bra</span>";
			
		} else {		
			choice_arr.sort(function(a, b) {
				return b[1] - a[1];
			});

			for (i = 0; i < choice_arr.length; i++) {
				if (i == 0) {
					// Just add the choice text
					order_description_string += choice_arr[i][0];
				} else {
					if ((choice_arr[i-1][1] - choice_arr[i][1]) == 0) {
						// Just add a comma, no ranking text
						order_description_string += (', ' + choice_arr[i][0]);
					} else {
						// Add some ranking text
						var distance = choice_arr[i-1][1] - choice_arr[i][1];

						order_description_string += " <br><span style=\"font-weight: bold\">";

						if (choice_type == 'A') {
							order_description_string += $(document).data('ranking-text-type-a')[distance-1];
						} else if (choice_type == 'C') {
							order_description_string += $(document).data('ranking-text-type-c')[distance-1];
						}

						order_description_string += '</span> ';
						order_description_string += choice_arr[i][0];
					}
				}
			}
		}

		order_description.html(order_description_string);

	} else if (choice_type == 'B') {
		var scale_list = $(choice_list).siblings("ul[class*='ranked-choice-scale-list']");
		var arrow_item = $(choice_list).children('li:first-child');
		var arrow_item_position = $(arrow_item).attr('data-position');
		var ranking_distance = Math.abs(arrow_item_position);
		var choices = [scale_list.children('li:first-child').children('span:first-child').text(),
		scale_list.children('li:last-child').children('span:first-child').text()];

		if (arrow_item_position > 0) {
			// Switch the order of the two choices
			choices[0] = scale_list.children('li:last-child').children('span:first-child').text();
			choices[1] = scale_list.children('li:first-child').children('span:first-child').text();
		}

		order_description.html(choices[0] + " <span style=\"font-weight: bold\">" + $(document).data('ranking-text-type-b')[ranking_distance] + " </span>" + choices[1]);
	}
}

function removeRankedChoice(element) {
	var choice_list = $(element).closest('ul.ranked-choice-list');
	var choice_list_items = choice_list.children('li');
	var to_be_removed_item = $(element).closest('li');
	var choice_type = $(choice_list).siblings("svg[class*='ranked-choice-slider-bar']").attr('data-choice-type');
	var order_description = $(choice_list).siblings("p[class*='ranked-choice-order-description']");

	choice_list_items.each(function(index, element) {
		var slider_drag_square = $(choice_list_items[index]).find("div[class*='ranked-choice-slider-drag-handle']");
		var ranked_choice_polygon = $(choice_list_items[index]).find("svg[class*='ranked-choice-polygon']");
		var old_margin_bottom = parseInt(slider_drag_square.css('margin-bottom'));
		var new_margin_bottom = 0;

		if (index < to_be_removed_item.index()) {
			new_margin_bottom = old_margin_bottom + to_be_removed_item.outerHeight();
			ranked_choice_polygon.css('height', ranked_choice_polygon.outerHeight() - to_be_removed_item.outerHeight());
		} else if (index > to_be_removed_item.index()) {
			new_margin_bottom = old_margin_bottom;
		}

		slider_drag_square.css('margin-bottom', new_margin_bottom);
	});

	to_be_removed_item.remove();
	updateRankedChoiceSliderBarGradient(choice_list_items[0], choice_list.siblings("svg[class*='ranked-choice-slider-bar']"));
	updateRankedChoiceOrderDescription(choice_type, order_description, choice_list);

	if (choice_list.children().length <= 2) {
		choice_list.children('li:nth-child(2)').find("a[class*='ranked-choice-remove']").css('display', 'none');
	}

	updateRankedChoiceOrderDescription(choice_type, order_description, choice_list)
}

function adjustRankedChoicePolygons(element) {
	//var slider_bar = $(element).prev("div[class*='ranked-choice']").children("div[class*='ranked-choice-slider-bar']");
	//slider_bar.children("div[class*='ranked-choice-polygon']").css('height', slider_bar.
}

function isInPreviewMode() {
	return $("#toolbar button[onclick='preview()']").hasClass('active');	
}

function submitQuestionnaire() 
{
	// readData();
	// saveData();
	var questionnaire_data = new Array();

	$(".frm-question").each(function(index, element) {
		if ($(element).css('display') != 'none') {
			var question_type = $(element).children('select:first-child').attr('data-current-question-type');
			var question_obj = {type: question_type};

			if (question_type == 'short-answer') {
				question_obj.data = $(element).find("input[class*='short-answer-text']").val();
			} else if (question_type == 'paragraph') {
				question_obj.data = $(element).find("div[class*='textarea']").text();
			} else if (question_type == 'checkboxes') {
				question_obj.data = new Array();

				$(element).children("div[class*='question-type-checkboxes']").children("div[class*='alternative']").each(function(alternative_index, alternative_element) {
					var checked = $(alternative_element).children("input[type='checkbox']").prop('checked');
					question_obj.data.push(checked == true ? true : false);
				});	
			} else if (question_type == 'multiple-choice') {
				question_obj.data = new Array();

				$(element).children("div[class*='question-type-checkboxes']").children("div[class*='alternative']").each(function(alternative_index, alternative_element) {
					var checked = $(alternative_element).children("input[type='radio']").prop('checked');
					question_obj.data.push(checked == true ? true : false);
				});
			} else if (question_type == 'ranked-choice') {
				question_obj.data = new Array();

				$(element).children("div[class*='question-type-ranked-choice']").children("ul[class*='ranked-choice-list']").children('li:not(:first-child)').each(function(choice_index, choice_element) {
					var choice_position = $(choice_element).attr('data-position');
					question_obj.data.push(choice_position);
				});				
			}

			questionnaire_data.push(question_obj);
		}
	});
}

function addAnswerAlternative(element) 
{
	var alternatives = $(element).siblings("div[class*='alternative']");
	var previous_alternative = alternatives.last();
	var cloned_alternative = previous_alternative.clone();
	var question_number = $(element).closest("div[class*='section']").index();
	var alternative_name = 'question_' + question_number;
	var alternative_id = alternative_name + '_alt_' + (alternatives.length + 1);

	$(cloned_alternative).children('input').attr({'name': alternative_name, 'id': alternative_id});
	$(cloned_alternative).children('label').attr('for', alternative_id);

	if (alternatives.length == 1) {
		previous_alternative.children("span[class*='question-remove-alternative']").css('visibility', 'visible');
		cloned_alternative.children("span[class*='question-remove-alternative']").css('visibility', 'visible');
	}

	$(element).before(cloned_alternative);
}

function removeQuestionAlternative(element) {
	var alternatives = $(element).parent().siblings("div[class*='alternative']");

	if (alternatives.length <= 2) {
		alternatives.first().children("span[class*='question-remove-alternative']").css('visibility', 'hidden');
	}

	$(element).parent().remove();
}

function switchFocusedSection(event) {
	if (isInPreviewMode() == false) {
		var to_be_focused_form = $(event.target).closest("div[class*='section']");
		var currently_focused_form = $('.focused');

		currently_focused_form.removeClass('focused');
		to_be_focused_form.addClass('focused');
	}
}

function addQuestion() {
	var question_form = $(".frm-question[style*='display: none']");
	var new_question_form = $(question_form).clone();
	var question_number = $("#frm-questionnaire > div[class*='section']").length;
	var alternative_name = 'question_' + question_number;
	var alternative_id = alternative_name + '_alt_1';
	var question_type_select = $(new_question_form).children('select');
	var question_type_form_selector = '.question-type-' + $(question_type_select).val() + "[style*='display: none']";
	var cloned_question_type_form = $(question_type_form_selector).clone();

	$(new_question_form).css('display', 'block');		
	$(cloned_question_type_form).css('display', 'block');

	$(question_type_select).attr('data-current-question-type', $(question_type_select).val());

	$(new_question_form).children('.form-operations-list').before(cloned_question_type_form);

	$(new_question_form).css('z-index', question_number);

	$('#btn-submit-questionnaire').before(new_question_form);

	if (question_type_select.val() == 'multiple-choice' || question_type_select.val() == 'checkboxes') {
		cloned_question_type_form.find('input').attr({'name': alternative_name, 'id': alternative_id});
		cloned_question_type_form.find('label').attr('for', alternative_id);
	} else if (question_type_select.val() == 'ranked-choice') {
		var choice_list = cloned_question_type_form.find('.ranked-choice-list');
		var ranked_choice = choice_list.children('li:not(:first-child)');
		var slider_drag_square = ranked_choice.children('.ranked-choice-slider-drag-handle');
		var ranked_choice_polygon = ranked_choice.children('.ranked-choice-polygon');

		slider_drag_square.css('margin-bottom', '-' + (choice_list.outerHeight() - ranked_choice.outerHeight()) + 'px');
		ranked_choice_polygon.css('height', choice_list.outerHeight() + 'px');
	}

	switchFocusedSection(new_question_form);
}

function changeQuestionType(element) {
	var current_question_type = $(element).attr('data-current-question-type');
	var new_question_type = $(element).val();
	var newly_selected_question_type  = "option[value='" + new_question_type + "']";

	$(element).children("option[selected='selected']").removeAttr('selected');
	$(newly_selected_question_type).attr('selected', 'selected');

	if (current_question_type == 'checkboxes' && new_question_type == 'multiple-choice') {
		// Replace checkboxes with radio buttons
		$(element).siblings("div[class*='question-type']").find("input[type='checkbox']").attr('type', 'radio');
	} else if (current_question_type == 'multiple-choice' && new_question_type == 'checkboxes') {
		// Replace radio buttons with checkboxes
		$(element).siblings("div[class*='question-type']").find("input[type='radio']").attr('type', 'checkbox');
	} else {
		var new_question_type_form_selector = '.question-type-' + new_question_type + "[style*='display: none']";
		var cloned_question_type_form = $(new_question_type_form_selector).clone();
		$(cloned_question_type_form).css('display', 'block');

		$(element).siblings("div[class*='question-type']").replaceWith(cloned_question_type_form);

		if (new_question_type == 'ranked-choice') {			
			var choice_list = cloned_question_type_form.find('.ranked-choice-list');
			var ranked_choice = choice_list.children('li:not(:first-child)');
			var slider_drag_square = ranked_choice.children('.ranked-choice-slider-drag-handle');
			var ranked_choice_polygon = ranked_choice.children('.ranked-choice-polygon');

			slider_drag_square.css('margin-bottom', '-' + (choice_list.outerHeight() - ranked_choice.outerHeight()) + 'px');
			ranked_choice_polygon.css('height', choice_list.outerHeight() + 'px');
		}
	}

	$(element).attr('data-current-question-type', new_question_type);
}

function removeForm(element) {
	$(element).closest("div[class*='section']").remove();
}

function duplicateForm(element) {
	var question = $(element).closest("div[class*='section']");
	var cloned_question = $(question).clone(true, true);

	question.after(cloned_question);
	$(cloned_question).focus();
}

function moveForm(element, direction) 
{
	var form = $(element).closest("div[class*='section']");
	var sibling_form = null;

	if (direction == 'up') {
		sibling_form = $(form).prev("div[class*='section']:not([id='questionnaire-header'])");

		if (sibling_form.length > 0) {
			$(sibling_form).before(form);
		}
	} else if (direction == 'down') {
		sibling_form = $(form).next("div[class*='section']");

		if (sibling_form.length > 0) {
			$(sibling_form).after(form);
		}
	}
}

function addHeadingAndDescription() {
	var heading_and_description_form = $("div.frm-heading-and-description[style*='display: none']");
	var new_heading_and_description_form = $(heading_and_description_form).clone();

	$(new_heading_and_description_form).css('display', 'block');

	$('#btn-submit-questionnaire').before(new_heading_and_description_form);

	switchFocusedSection(new_heading_and_description_form);
}

function preview() {
	var preview_button = $("#toolbar button[onclick='preview()']");
	var text_areas = $('#questionnaire div.textarea');

	preview_button.toggleClass('active');

	if (isInPreviewMode() == true) {
		// Preview mode ON
		$("#questionnaire input[class*='questionnaire-title']").prop('disabled', true);
		$("#toolbar button:not([onclick='preview()'])").prop('disabled', true);

		// Heading and description form
		$("#questionnaire input[class*='frm-heading-and-description-heading']").prop('disabled', true);

		// Question form
		$("#questionnaire input[class*='frm-question-question-title']").prop('disabled', true);
		$('#questionnaire .frm-question-question-description').attr('contenteditable', 'false');
		
		// Question type multiple choice
		$('.question-remove-alternative').hide();

		$("#questionnaire .form-operations-list, select, #questionnaire button:not([id='btn-submit-questionnaire'])").slideUp();

		// Question type ranked choice
		$("#questionnaire div[class*='question-type-ranked-choice']").each(function(index, element) {
		
		var ranked_choice_choice_type_label = $(element).children("label[class*='ranked-choice-choice-type-label']");
		var ranked_choice_order_description = $(element).children("p[class*='ranked-choice-order-description']");
		var ranked_choice_list = $(element).children("ul[class*='ranked-choice-list']");

		updateRankedChoiceOrderDescription(ranked_choice_choice_type_label.children('select').val(),
		ranked_choice_order_description,
		ranked_choice_list);
		});

		text_areas.each(function(index, element) {
		$(element).css('resize', 'none');
		$(element).attr('contenteditable', 'false');
		});

		$('#questionnaire ul.ranked-choice-scale-list span').attr('contenteditable', 'true');


		
		$("#questionnaire div.question-type-checkboxes input[type='checkbox']").removeAttr('disabled');
		$("#questionnaire div.question-type-multiple-choice input[type='radio']").removeAttr('disabled');
		$("#questionnaire div.question-type-checkboxes input[type='checkbox']").siblings('label').attr('contenteditable', 'false');
		$("#questionnaire div.question-type-multiple-choice input[type='radio']").siblings('label').attr('contenteditable', 'false');
		
		
		$('.ranked-choice-list > li:not(:first-child) .ranked-choice-text').attr('contenteditable', 'false').css('cursor', 'ew-resize');
		$('.ranked-choice-scale-list span').attr('contenteditable', 'false');
		$('.ranked-choice-remove').css('display', 'none');
		$('.ranked-choice-order-description').css('display', 'block');
		$('.ranked-choice-choice-type-label').css('display', 'none');

		$('.ranked-choice-text').each(function(index, element) {
			$(element).on('mousedown', rankedChoiceOnMouseDown)
		});
	} else {
		// Preview mode OFF
		$("#questionnaire input[class*='questionnaire-title']").prop('disabled', false);
		$("#toolbar button:not([onclick='preview()'])").prop('disabled', false);

		// Heading and description form
		$("#questionnaire input[class*='frm-heading-and-description-heading']").prop('disabled', false);

		// Question form
		$("#questionnaire input[class*='frm-question-question-title']").prop('disabled', false);
		$('#questionnaire .frm-question-question-description').attr('contenteditable', 'true');
		
		// Question type multiple choice
		$('.question-remove-alternative').show();

		$("#questionnaire .form-operations-list, select, #questionnaire button:not([id='btn-submit-questionnaire'])").slideDown();

		text_areas.each(function(index, element) {
			$(element).css('resize', 'vertical');
			$(element).attr('contenteditable', 'true');
		});

		text_areas.css('resize', 'vertical');
		text_areas.prop('disabled', false);
		text_areas.attr('contenteditable', 'true');

		$('#questionnaire ul.ranked-choice-scale-list span').attr('contenteditable', 'true');

		$("#questionnaire div.question-type-checkboxes input[type='checkbox']").attr('disabled', 'disabled');
		$("#questionnaire div.question-type-multiple-choice input[type='radio']").attr('disabled', 'disabled');
		$("#questionnaire div.question-type-checkboxes input[type='checkbox']").siblings('label').attr('contenteditable', 'true');
		$("#questionnaire div.question-type-multiple-choice input[type='radio']").siblings('label').attr('contenteditable', 'true');
		
		
		$('.ranked-choice-list > li:not(:first-child) .ranked-choice-text').attr('contenteditable', 'true').css('cursor', 'auto').off('mousedown');
		$('.ranked-choice-scale-list span').attr('contenteditable', 'true');
		$('.ranked-choice-remove').css('display', 'inline');
		$('.ranked-choice-order-description').css('display', 'none');
		$('.ranked-choice-choice-type-label').css('display', 'inline');
	}

	$('#btn-submit-questionnaire').toggle();
}

function clearQuestionnaire() {
	var result = confirm("Är du säker på att du vill rensa hela formuläret?");

	if (result == true) {
		// Clear the questionnaire
		$('#questionnaire div.frm-heading-and-description').remove();
		$('#questionnaire div.frm-question').remove();
	}
}

function saveQuestionnaire() {
	var session_save_counter = $('#frm-questionnaire').attr('data-session-save-counter');

	if (session_save_counter == null) {
		session_save_counter = 1;
	} else {
		session_load_counter++;
	}

	$("#questionnaire input[type='text']").each(function(index, element) {
		$(element).attr('value', $(element).val());
	});
	
	var questionnaire_raw_html = $('#frm-questionnaire').html();
	var obj = {userId: 3, data: questionnaire_raw_html};
	
	Shiny.onInputChange("save_questionnaire", obj);
	$('#frm-questionnnaire').attr('data-session-save-counter', session_save_counter);
}

function saveQuestionnaireHandler(response) {
	alert("Formulär sparat!");
}

function loadQuestionnaire() {
	var session_load_counter = $('#frm-questionnaire').attr('data-session-load-counter');

	if (session_load_counter == null) {
		session_load_counter = 1;
	} else {
		session_load_counter++;
	}

	var questionnaire_raw_html = $('#frm-questionnaire').html();
	var obj = {userId: 3, data: questionnaire_raw_html};
	
	Shiny.onInputChange("load_questionnaire", obj);
	$('#frm-questionnaire').attr('data-session-load-counter', session_load_counter);
}

function loadQuestionnaireHandler(response) {
	$('#frm-questionnaire').html(response);
	alert("Formulär laddat!");
}