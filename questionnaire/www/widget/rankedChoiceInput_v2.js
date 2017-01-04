function rankedChoiceInput_create(container, width, height, numberOfSliderSteps) {
	var paper = Raphael(container, width, height);
	
	paper.canvas.style.backgroundColor = 'rgb(245,245,100)';
	paper.customAttributes.numberOfSliderSteps = numberOfSliderSteps;
	paper.customAttributes.choices = new Array();
	
	// Create the slider bar
	paper.customAttributes.sliderBar = {borderRadius: 5,
										fillColor: 'rgb(230,230,230)',
										strokeColor: 'rgb(160,160,160)',
										strokeWidth: 1,
										x: 0,
										y: 50,
										width: width,
										height: 10};
	
	paper.customAttributes.sliderBar.instance = createSliderBar(paper,
												paper.customAttributes.sliderBar.x,
												paper.customAttributes.sliderBar.y,
												paper.customAttributes.sliderBar.width,
												paper.customAttributes.sliderBar.height);
	
	paper.customAttributes.choiceTextBackground = {borderRadius: 3,
												fillColor: 'rgba(0,0,0,.2)',
												strokeWidth: 0};
	
	paper.customAttributes.choiceText = {fillColor: 'rgb(0,0,0)',
										startingXCoord: width / 2,
										startingYCoord: 25};
	
	paper.customAttributes.sliderDragSquare = {borderRadius: 3,
										fillColor: '90-rgb(160,160,160)-rgb(255,255,255)',
										strokeColor: 'rgb(90,90,90)',
										strokeWidth: 1,
										width: 50,
										height: 20};

	paper.customAttributes.sliderDragSquare.instances = paper.set();
	
	return paper;
}

function rankedChoiceInput_addScaleText(rankedChoiceInput, scaleText) {
	rankedChoiceInput.text(50,50,scaleText);
}

choice_onDragMove = function(dx, dy, x, y, event) {
	if (this[0].dragStartX + dx >= this.paper.customAttributes.sliderBar.x && this[0].dragStartX + dx + this[0].attr('width') <= this.paper.customAttributes.sliderBar.width) {	
		this[0].attr('x', this[0].dragStartX + dx);
		this[1].attr('x', this[1].dragStartX + dx);
		this[2].attr('x', this[2].dragStartX + dx);
	}
}

choice_onDragStart = function(x, y, event) {
    this[0].dragStartX = this[0].attr('x');
	this[1].dragStartX = this[1].attr('x');
	this[2].dragStartX = this[2].attr('x');
}

choice_onDragEnd = function(event) {
	
}

function rankedChoiceInput_addChoice(rankedChoiceInput, choiceText) {
	var heightDiff = rankedChoiceInput.customAttributes.sliderDragSquare.height - rankedChoiceInput.customAttributes.sliderBar.height;
	var sliderDragSquareXCoord = (rankedChoiceInput.width / 2) - (rankedChoiceInput.customAttributes.sliderDragSquare.width / 2);
	var sliderDragSquareYCoord = rankedChoiceInput.customAttributes.sliderBar.instance.attr('y') - (heightDiff / 2);
	var choiceTextYCoord = 0;
	
	if (rankedChoiceInput_getChoicesCount(rankedChoiceInput) >= 1) {
		var lastChoice = rankedChoiceInput_getLastChoice(rankedChoiceInput);
		choiceTextYCoord = lastChoice[1].attr('y') + lastChoice[1].getBBox().height;
	} else {
		choiceTextYCoord = rankedChoiceInput.customAttributes.choiceText.startingYCoord;
	}
	
	var newChoice = rankedChoiceInput.set();
	
	newChoice.push(createSliderDragSquare(rankedChoiceInput, sliderDragSquareXCoord, sliderDragSquareYCoord));
	newChoice.push(createChoiceText(rankedChoiceInput, rankedChoiceInput.customAttributes.choiceText.startingXCoord, choiceTextYCoord, choiceText));
	newChoice.push(createChoiceTextBackground(rankedChoiceInput, newChoice[1]));
	
	rankedChoiceInput.customAttributes.sliderBar.instance.attr('y', rankedChoiceInput.customAttributes.sliderBar.instance.attr('y') + newChoice[1].getBBox().height);
	
	newChoice.drag(choice_onDragMove, choice_onDragStart, choice_onDragEnd, newChoice, newChoice);
	
	return rankedChoiceInput.customAttributes.choices.push(newChoice);
}

function rankedChoiceInput_removeChoice(rankedChoiceInput, choiceIndex) {
	// return rankedChoiceInput.customAttributes.choices.splice(choiceIndex, 1);
}

function rankedChoiceInput_getChoicesCount(rankedChoiceInput) {
	return rankedChoiceInput.customAttributes.choices.length;
}

function rankedChoiceInput_getChoices(rankedChoiceInput) {
	return rankedChoiceInput.customAttributes.choices;
}

function rankedChoiceInput_getFirstChoice(rankedChoiceInput) {
	if (rankedChoiceInput.customAttributes.choices.length >= 1) {
		return rankedChoiceInput.customAttributes.choices[0];
	} else {
		return null;
	}
}

function rankedChoiceInput_getLastChoice(rankedChoiceInput) {
	var lastIndex = rankedChoiceInput.customAttributes.choices.length - 1;
	
	if (lastIndex >= 0) {
		return rankedChoiceInput.customAttributes.choices[lastIndex];
	} else {
		return null;
	}
}

function createSliderBar(rankedChoiceInput, x, y) {
	var sliderBar = rankedChoiceInput.rect(x, y, rankedChoiceInput.customAttributes.sliderBar.width, rankedChoiceInput.customAttributes.sliderBar.height, rankedChoiceInput.customAttributes.sliderBar.borderRadius);

	sliderBar.attr({'fill': rankedChoiceInput.customAttributes.sliderBar.fillColor,
					'stroke': rankedChoiceInput.customAttributes.sliderBar.strokeColor,
					'stroke-width': rankedChoiceInput.customAttributes.sliderBar.strokeWidth});

	return sliderBar;
}

function createSliderDragSquare(rankedChoiceInput, x, y) {
	var sliderDragSquare = rankedChoiceInput.rect(x, y, rankedChoiceInput.customAttributes.sliderDragSquare.width, rankedChoiceInput.customAttributes.sliderDragSquare.height, rankedChoiceInput.customAttributes.sliderDragSquare.borderRadius);

	sliderDragSquare.attr({'fill': rankedChoiceInput.customAttributes.sliderDragSquare.fillColor,
							'stroke': rankedChoiceInput.customAttributes.sliderDragSquare.strokeColor,
							'stroke-width': rankedChoiceInput.customAttributes.sliderDragSquare.strokeWidth,
							'cursor': 'ew-resize'});
	
	rankedChoiceInput.customAttributes.sliderDragSquare.instances.push(sliderDragSquare);
	
	return sliderDragSquare;
}

function createChoiceText(rankedChoiceInput, x, y, text) {
	var choiceText = rankedChoiceInput.text(x, y, text);
	
	choiceText.attr({'fill': rankedChoiceInput.customAttributes.choiceText.fillColor,
					'cursor': 'ew-resize'});
	
	return choiceText;
}

function createChoiceTextBackground(rankedChoiceInput, choiceText) {
	var bbox = choiceText.getBBox();
	var choiceTextBackground = rankedChoiceInput.rect(bbox.x, bbox.y, bbox.width, bbox.height,
								rankedChoiceInput.customAttributes.choiceTextBackground.borderRadius).toBack();
	
	choiceTextBackground.attr({'fill': rankedChoiceInput.customAttributes.choiceTextBackground.fillColor,
								'stroke-width': rankedChoiceInput.customAttributes.choiceTextBackground.strokeWidth,
								'cursor': 'ew-resize'});
								
	return choiceTextBackground;
}