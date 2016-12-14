createRankedChoiceInput = function(container, containerWidth, containerHeight, numSliderSteps, choicesText, scaleText, gradients) {
  var containerMiddleX = containerWidth / 2;
  var sliderBarHeight = 7;
  var sliderDragSquareWidth = 30;
  var sliderDragSquareHeight = 20;
  var choicesTextPadding = choicesText.padding == null ? [0,0] : choicesText.padding;
  
  var paper = Raphael(container, containerWidth, containerHeight);
  var stepDx = (containerMiddleX - (sliderDragSquareWidth / 2)) / ((numSliderSteps) / 2);


  var scaleTextSet = createScaleText(paper, containerWidth, scaleText);
  var choicesTextSet = createChoicesText(paper, containerMiddleX,
                                                scaleTextSet.getBBox().y + scaleTextSet.getBBox().height + 10,
                                                choicesText, choicesTextPadding);

  var choicesTextSetBBox = choicesTextSet.getBBox();

  var sliderBar = createSliderBar(paper, choicesTextSetBBox.width / 2,
                                        choicesTextSetBBox.y + choicesTextSetBBox.height + 30,
                                        containerWidth - (2 * (choicesTextSetBBox.width / 2)),
                                        sliderBarHeight);

  var sliderBarBBox = sliderBar.getBBox();



  stepDx = (sliderBarBBox.width / (numSliderSteps-1));


  choicesTextSet.translate(-((sliderBarBBox.width / 2) - (sliderDragSquareWidth / 2)), 0);

  scaleTextSet[0].attr('x', choicesTextSetBBox.width / 2);
  scaleTextSet[scaleTextSet.length-1].attr('x', sliderBarBBox.x + sliderBarBBox.width);

  //createScaleMidPointLine(paper, containerMiddleX, sliderBarBBox.y, sliderBarHeight);

  var sliderDragSquares = createSliderDragSquares(paper,
                              sliderBarBBox.x - (sliderDragSquareWidth / 2), // containerMiddleX - sliderDragSquareWidth / 2,
                              sliderBarBBox.y - (sliderDragSquareHeight / 2) + 3,
                              sliderDragSquareWidth,
                              sliderDragSquareHeight,
                              choicesTextSet.length);

  var choicesTextBackgroundSet = createChoicesTextBackgrounds(paper, choicesTextSet, choicesTextPadding);
  var choicesPolygonSet = createChoicesPolygons(paper, sliderBarBBox.x, sliderBarBBox.y + (sliderBarHeight / 2), choicesTextBackgroundSet);

  var choiceId = 0;
  var choiceArr = new Array();
  var choicesSet = new Array();

  for (i = 0; i < choicesText.strings.length; i++) {
    var choiceSet = paper.set();
    choiceSet.sliderBarX = sliderBarBBox.x;
    choiceSet.sliderBarWidth = sliderBarBBox.width
    choiceSet.gradients = gradients;
    choiceSet.stepDx = stepDx;
    choiceSet.xMove = 0;

    var sliderDragSquare = sliderDragSquares.pop();
    var choiceText = choicesTextSet.pop();
    var choiceTextBackground = choicesTextBackgroundSet.pop();
    var choicePolygon = choicesPolygonSet.pop();
	
    sliderDragSquare.onHoverInStrokeColor = '#000';;
    sliderDragSquare.onHoverOutStrokeColor = sliderDragSquare.attr('stroke');

    sliderDragSquare.choiceId = choiceId;
    choiceText.choiceId = choiceId;
    choiceTextBackground.choiceId = choiceId;
    choicePolygon.choiceId = choiceId;

    choiceSet.push(sliderDragSquare);
    choiceSet.push(choiceText);
    choiceSet.push(choiceTextBackground);
    choiceSet.push(choicePolygon);

    choiceSet.currentStep = choicesText.positions[i];
    choiceSet.currentX = (choiceSet.currentStep - 1) * stepDx;

    choiceSet.hover(choiceOnHoverIn, choiceOnHoverOut, sliderDragSquare, sliderDragSquare);
    choiceSet.drag(choiceOnDragMove, choiceOnDragStart, choiceOnDragEnd,choiceSet, choiceSet);

    choiceId++;
    choiceArr.push(sliderDragSquare);
    choicesSet.push(choiceSet);
    choiceSet.translate(choiceSet.currentX, 0);
	
  }

  var leftmostChoice = choicesSet[0];
  var rightmostChoice = choicesSet[0];

  for (i = 0; i < choicesSet.length; i++) {
    if (choicesSet[i].currentStep > rightmostChoice) {
      rightmostChoice = choicesSet[i];
    } else if (choicesSet[i].currentStep < leftmostChoice) {
      leftmostChoice = choicesSet[i];
    }
  }

  Raphael.choiceArr = choiceArr;
  Raphael.choicesSet = choicesSet;
  Raphael.leftmostChoice = leftmostChoice;
  Raphael.rightmostChoice = rightmostChoice;
};

createScaleMidPointLine = function(paper, x, y, height) {
  paper.path("M " + x + "," + y + " l0," + height).attr({'stroke': 'rgba(255,255,255,.8)', 'stroke-width': '3'});
}

createScaleText = function(paper, containerWidth, scaleText) {
  if (scaleText == null) {
    return null;
  }

  var scaleTextSet = paper.set();

  for (i = 0; i < scaleText.strings.length; i++) {
    var textAnchor = 'middle';

    if (scaleText.positions[i] == 0) {
      textAnchor = 'start';
    } else if (scaleText.positions[i] == 100) {
      textAnchor = 'end';
    }

    var x = (containerWidth * scaleText.positions[i]) / 100;

    var scaleTextDrawn = paper.text(x, 0, scaleText.strings[i]).attr({'text-anchor': textAnchor, 'font-family': scaleText.font_family});
    scaleTextSet.push(scaleTextDrawn);
  }

  if (scaleText.font_size) {
    scaleTextSet.attr('font-size', scaleText.font_size);
  }

  if (scaleText.font_family) {
    scaleTextSet.attr('font-family', scaleText.font_family);
  }

  scaleTextSet.attr('y', scaleTextSet.getBBox().height / 2);
  
  return scaleTextSet;
};

choiceOnHoverIn = function() {
  if (!this.paper.is_dragging) {
    this.attr('stroke', this.onHoverInStrokeColor);
  }
};

choiceOnHoverOut = function() {
  if (!this.paper.is_dragging) {
    this.attr('stroke', this.onHoverOutStrokeColor);
  }
};

choiceOnDragMove = function(dx, dy, x, y, event) {
  if (this.currentX + dx >= 0 && this.currentX + dx <= this.sliderBarWidth){
  var relXCoordsFromDragStart = this.currentX - (this[0].dragStartX - dx);

  if (Math.abs(relXCoordsFromDragStart) > (this.stepDx / 2)) {
    // Move!
    var xMove = 0;

    if (relXCoordsFromDragStart > 0) {
      this[0].dragStartX += this.stepDx;
      this.currentStep += 1;
      xMove = this.stepDx;
    } else {
      this[0].dragStartX -= this.stepDx;
      this.currentStep -= 1;
      xMove = -this.stepDx;
    }

    this.xMove += xMove;
    this.translate(xMove, 0);

    updateSliderBarBackground(this);
  }}
};

choiceOnDragStart = function(x, y) {
  for (i = 0; i < 3; i++) {
    this[i].dragStartX = this[i].getBBox().x + (this[i].getBBox().width / 2) - this.sliderBarX;
  }

  this[3].dragStartX = this[3].getBBox().x - (this[3].getBBox().width / 2) - this.sliderBarX;

  this.xMove = 0;
  this.paper.is_dragging = true;
  this.paper.draggingChoice = this.choiceId;
};

updateSliderBarBackground = function(choiceSet) {
  var sliderBarColor = "0-#fff:20-#f00:20-#fff:50-#006400:80-#fff:80"; //'rgb(240,240,240)';

  for (i = 0; i < choiceSet.gradients.positions.length; i++) {
    var gradientDirection = choiceSet.gradients.positions[2] - choiceSet.gradient.positions[1];

    if (gradientDirection > 0) {

    } else {

    }
  }
}

choiceOnDragEnd = function(event) {
  this.paper.is_dragging = false;
  var elementByPoint = this.paper.getElementByPoint(event.clientX, event.clientY);

  if (elementByPoint == null || elementByPoint.choiceId == null) {
    this[0].attr('stroke', this[0].onHoverOutStrokeColor);
  } else if (elementByPoint.choiceId != this[0].choiceId) {
      this[0].attr('stroke', this[0].onHoverOutStrokeColor);
      Raphael.choiceArr[elementByPoint.choiceId].attr('stroke', this[0].onHoverInStrokeColor);
  }

  this.currentX += this.xMove;
};

createSliderDragSquares = function(paper, x, y, width, height, numberOfChoices) {
  var sliderDragSquareSet = paper.set();
  var sliderDragSquareColor = '90-rgb(160,160,160)-rgb(255,255,255)';
  var sliderDragSquareStrokeColor = 'rgb(90,90,90)';
  var sliderDragSquareBorderRadius = 3;

  for (i = 0; i < numberOfChoices; i++) {
    var sliderDragSquare = paper.rect(x, y, width, height, sliderDragSquareBorderRadius);
    sliderDragSquareSet.push(sliderDragSquare);
  }

  sliderDragSquareSet.attr({fill: sliderDragSquareColor, stroke: sliderDragSquareStrokeColor, 'stroke-width': 1, 'cursor': 'ew-resize'});
  
  return sliderDragSquareSet;
};

createSliderBar = function(paper, x, y, sliderBarWidth, sliderBarHeight) {
  //var sliderBarColor = "90-rgb(140,140,140)-rgb(80,80,80)"; //'rgb(240,240,240)';

  var sliderBarColor = "0-#fff:20-#f00:20-#fff:50-#006400:80-#fff:80"; //'rgb(240,240,240)';
  var sliderBarStrokeColor = 'rgb(140,140,140)';
  var sliderBarBorderRadius = 3;

  var sliderBar = paper.rect(x, y, sliderBarWidth, sliderBarHeight, sliderBarBorderRadius);
  
  sliderBar.attr({fill: sliderBarColor, stroke: sliderBarStrokeColor, 'stroke-width': 1});

  return sliderBar;
};

createChoicesTextBackgrounds = function(paper, choicesTextSet, choicesTextPadding) {
  var choicesTextBackgroundSet = paper.set();
  var choicesTextBackgroundMaxWidth = 0;
  var choicesTextBackgroundMinX = Number.MAX_VALUE;
  var choicesTextBackgroundColor = '90-rgb(170,170,170)-lightgray'; //'90-rgb(170,170,170)-rgb(220,220,220)';
  var choicesTextBackgroundStrokeColor = 'rgba(255,255,255,.7)';

  choicesTextSet.forEach(function(obj) {
    var bbox = obj.getBBox();
    var choicesTextBackgroundBorderRadius = 6;

    var choiceTextBackground = paper.rect(bbox.x - choicesTextPadding[0],
                                bbox.y - choicesTextPadding[1],
                                bbox.width + (2 * choicesTextPadding[0]),
                                bbox.height + (2 * choicesTextPadding[1]),
                                choicesTextBackgroundBorderRadius);

    choiceTextBackground.attr('stroke-width', 1);
                              
    choicesTextBackgroundSet.push(choiceTextBackground);

    if (choiceTextBackground.attr('width') > choicesTextBackgroundMaxWidth) {
      choicesTextBackgroundMaxWidth = choiceTextBackground.attr('width');
    }

    if (choiceTextBackground.attr('x') < choicesTextBackgroundMinX) {
      choicesTextBackgroundMinX = choiceTextBackground.attr('x');
    }
  });

  choicesTextBackgroundSet.toBack();
  choicesTextBackgroundSet.attr({'x': choicesTextBackgroundMinX,
                                'fill': choicesTextBackgroundColor,
                                'width': choicesTextBackgroundMaxWidth,
                                'stroke': choicesTextBackgroundStrokeColor,
                                'cursor': 'ew-resize'});

  return choicesTextBackgroundSet;
};

createChoicesText = function(paper, x, y, choicesText, choicesTextPadding) {
  var choicesTextSet = paper.set();
  var maxChoicesTextDrawnWidth = 0;

  for (i = 0; i < choicesText.strings.length; i++) {
    var choiceTextDrawn = paper.text(x, y + choicesTextPadding[1], choicesText.strings[i]).attr(
                          {'text-anchor': 'middle',
                          'font-size': choicesText.font_size,
                          'font-family': choicesText.font_family,
                          'fill': 'black',
                          'cursor': 'ew-resize'});

    var choiceTextDrawnWidth = choiceTextDrawn.getBBox().width;
    var choiceTextDrawnHeight = choiceTextDrawn.getBBox().height;

    choicesTextSet.push(choiceTextDrawn);

    if (choiceTextDrawnWidth > maxChoicesTextDrawnWidth) {
      // Update the max width of the choice texts to current choice text
      maxChoicesTextDrawnWidth = choiceTextDrawnWidth;
    }

    y += choiceTextDrawnHeight + 2*choicesTextPadding[1];
  }

  return choicesTextSet;
};

createChoicesPolygons = function(paper, x, y, choicesTextBackgroundSet) {
  var choicesPolygonSet = paper.set();

  choicesTextBackgroundSet.forEach(function(obj) {
      var bbox = obj.getBBox();
      var choicePolygon = createChoicePolygon(paper, x, bbox.y, 10, y - bbox.y);
      choicesPolygonSet.push(choicePolygon);
  });


  choicesPolygonSet.attr({fill: 'rgba(0,0,0,.2)', 'stroke-width': 0, 'cursor': 'ew-resize'});
  choicesPolygonSet.toBack();

  return choicesPolygonSet;
};

createChoicePolygon = function(paper, x, y, width, height) {
  var pathString = "M " + (x - (width / 2)) + "," + y + 
                  " l" + width + ",0" + 
                  " l-" + (width / 2) + "," + height + 
                  " l-" + (width / 2) + ",-" + height;

  return paper.path(pathString);
};

rankedChoice_addChoice = function(choice) {

}

rankedChoice_removeChoice = function(choice) {
	
}

getValue = function(el) {
  var stepValues = new Array();

  for (i = 0; i < Raphael.choicesSet.length; i++) {
    stepValues.push(Raphael.choicesSet[i].currentStep);
  }

  return JSON.stringify(stepValues);
};

setValue = function(el, value) {
  var obj = JSON.parse(value); // Convert JSON text to Javascript object
};