////////////////////////////////////////////////////////////////////////////////
//// General functions
////////////////////////////////////////////////////////////////////////////////

forceArray = function(x) { return Array.isArray(x) ? x : [x]; };

hexPath = function(radius) {
  var inner = radius * Math.sqrt(3) * 0.5, halfradius = radius * 0.5;
  return "m0," + radius + 
          " l" + inner + "," + (-halfradius) +
          " l0," + (-radius) + " l" + (-inner) + "," + (-halfradius) +
          " l" + (-inner) + "," + (halfradius) + 
          " l0," + radius + " l" + inner + "," + halfradius;
};

////////////////////////////////////////////////////////////////////////////////
//// The widget
////////////////////////////////////////////////////////////////////////////////

HTMLWidgets.widget({

  name: 'aweSOMwidget',

  type: 'output',

  factory: function(el, width, height) {

    return {
           renderValue: function(data) {

    // Quit if no data
    if(data == null ) {return;}
    //console.log(data);
    
    // Import common data
    var plotType= data.plotType, 
  	  nbRows= data.gridInfo.nbLines, 
  	  nbColumns= data.gridInfo.nbColumns, 
  	  topology= data.gridInfo.topology, 
  	  superclass = data.superclass, 
  	  superclassColor = data.superclassColor, 
  	  cellNames = data.cellNames, 
  	  cellPop = data.cellPop,
  	  nVars = data.nVars, 
  	  label = forceArray(data.label), 
  	  labelColor = forceArray(data.labelColor), 
  	  normalizedValues = data.normalizedValues, 
  	  realValues = data.realValues, 
  	  normalizedSize = data.normalizedSize, 
  	  normalizedExtremesValues= data.normalizedExtremesValues, 
  	  realExtremesValues= data.realExtremesValues, 
  	  isCatBarplot = data.isCatBarplot, 
  	  showSC = data.showSC, 
  	  showAxes = data.showAxes, 
  	  transparency = data.transparency, 
  	  showNames = data.showNames, 
  	  legendPos = data.legendPos, 
  	  legendFontsize = data.legendFontsize, 
  	  legendReverse = data.legendReverse,
  	  clustering = data.clustering, 
  	  cloudColor = data.cloudColor, 
  	  colorVarName = data.colorVarName, 
  	  obsDetail = data.obsDetail, 
  	  fullData = data.fullData, 
  	  fullDataNames = data.fullDataNames;
    
    // IDs of plot, legend, infos
    var plotId = el.attributes.id.value;
    var infoId= plotId + "-info", 
      messageId= plotId + "-message", 
      namesId= plotId + "-names";

  	// Set widget size and cell size
  	width= data.sizeInfo;
  	height= data.sizeInfo;
  	var widgetWidth = width;
  	var widgetHeight = height;
  	var cellSize = Math.min(height / nbRows, width / nbColumns);

    document.getElementById(infoId).style.textAlign = "center";
    document.getElementById(messageId).style.textAlign = "center";
    document.getElementById(messageId).style.whiteSpace = "nowrap";
    document.getElementById(namesId).style.textAlign = "center";
    if (!showNames) document.getElementById(namesId).style.display = "none";
    document.getElementById(plotId).innerHTML = ""; //remove the old graph
    document.getElementById(infoId).innerHTML = "Hover over the plot for information.";
    document.getElementById(messageId).innerHTML = "-";
    document.getElementById(namesId).innerHTML = "-";
    
    /////////////////////////
    // Static download handlers
    /////////////////////////
/*    function downloadPng(link, filename) {
      var svg = document.getElementById(plotId).children[0];
      svg.toDataURL("image/png", {
          callback: function(data) {
            link.href = data;
            link.download = filename;
          }
      });
    }
    downPngEl = document.getElementById("downloadPng");
    if(typeof(downPngEl) != 'undefined' && downPngEl != null){
      downPngEl.addEventListener('click', function() {
        downloadPng(this, 'somplot.png');}, false);
    }*/
    
    function downloadSvg(link, filename) {
      var svg = document.getElementById(plotId).children[0];
      svg.toDataURL("image/svg+xml", {
          callback: function(data) {
            link.href = data;
            link.download = filename;
          }
      });
    }
    downSvgEl = document.getElementById("downloadSvg");
    if(typeof(downSvgEl) != 'undefined' && downSvgEl !== null){
      downSvgEl.addEventListener('click', function() {
        downloadSvg(this, 'somplot.svg');}, false);
    }


    /////////////////////////
    // Create rectangular or hexagonal grid
    /////////////////////////
    
    var cellPositions, innerCellSize, hexRadius;  

    if(topology.localeCompare('rectangular')==0){
      cellPositions= [];
      for (var theRow = 0 ; theRow < nbRows ; theRow++) {
        for (var theCol = 0 ; theCol < nbColumns ; theCol++) {
          var iCell = theRow*nbColumns + theCol;
          cellPositions[iCell] = [];
          cellPositions[iCell].x = (theCol + 0.5) * cellSize;
          cellPositions[iCell].y = (nbRows - 0.5 - theRow) * cellSize;
          cellPositions[iCell].cell = iCell;
          cellPositions[iCell].row = theRow;
          cellPositions[iCell].col = theCol;
        }
      }
      
      // inner cell size, area used in plots
      innerCellSize = 0.95 * cellSize;
      
      // new width and height for rect plot
      width = Math.min(widgetWidth, nbColumns * cellSize);
      height= Math.min(widgetHeight, nbRows * cellSize);
      
    } else if(topology.localeCompare('hexagonal')==0){
      // Compute sizes : hexRadius is the outer hex radius (ie side),
      // chosen to fit the dimensions of the svg.
      hexRadius = Math.min(widgetWidth / (Math.sqrt(3) * (nbColumns + 0.5)), 
                           widgetHeight / (1.5 * nbRows + 0.5));

      // inner cell size, area used in plots
      innerCellSize = 1.95 * hexRadius;
      
      // new width and height for hex plot
      width = Math.min(widgetWidth, hexRadius * Math.sqrt(3) * (nbColumns + 0.5));
      height= Math.min(widgetHeight, hexRadius * (1.5 * nbRows + 0.5));
      
      //Calculate the center positions of each hexagon
      cellPositions = [];
      for (var i = nbRows; i > 0; i--) {
        for (var j = 0; j < nbColumns; j++) {
          cellPositions.push({x: hexRadius * Math.sqrt(3) * 0.5 * (1 + 2 * j + ((nbRows-i+1) % 2)),
                              y: hexRadius * ((i-1) * 1.5 + 1), 
                              cell: (nbRows - i)*nbColumns + j, 
                              row: i, col:j}); 
        }
      }
    }

    // Create the svg containing plot and legend
    var theSvg = d3.select("#" + plotId).append("svg")
		.attr("width", width)
		.attr("height", height)
		.attr("style","display:block; margin:auto; margin-top:0px;");
		var thePlot = theSvg.append("g");

    // Create map cells
    var cells = thePlot.selectAll(".cell")
		.data(cellPositions)
		.enter().append("path")
		.attr("class", "cell")
		.attr("d", function (d) {
      if(topology.localeCompare('rectangular')==0){
        return "M" + d.x + "," + d.y + 
              " m" + (-cellSize / 2) + "," + (cellSize / 2) +
              " l" + cellSize + ",0" + " l" + "0," + (-cellSize) + 
              " l" + (-cellSize) + ",0" + " l" + "0," + cellSize; 
      } else if(topology.localeCompare('hexagonal')==0){
  			return "M" + d.x + "," + d.y + hexPath(hexRadius);
      }
		})
		.attr("stroke", function (d,i) {
			return "#fff";
		})
		.attr("stroke-width", cellSize / 100)
		.style("fill", function (d,i) {
			var indice = superclass[i];
			return superclassColor[indice-1];
		});

    // Cell mouse actions for all plots
    cells.on('mouseover', function(m, d) {
      if (transparency)
  			d3.select(this).transition().duration(10).style("fill-opacity", 0.8);
      d3.select("#" + infoId).text('Cell ' + parseInt(d.cell+1,10) + ', Superclass ' +
          superclass[d.cell] + ', N= ' + cellPop[d.cell]);
			d3.select("#" + namesId).text(cellNames[d.cell]);
    });
    cells.on('mouseout', function() {
      if (transparency)
        d3.select(this).transition().duration(400).style("fill-opacity", 1);
    });

    
    //////////////////////////////////////////////////////////////////////////
    // Plot background axes
    //////////////////////////////////////////////////////////////////////////
    if (showAxes) {
      if (plotType.localeCompare("Line")==0 || plotType.localeCompare("Barplot")==0 || plotType.localeCompare("Boxplot")==0) {
        if(topology.localeCompare('rectangular')==0){
          var xlims= [-0.4, 0.4], ylims = [-0.4, -0.2, 0.0, 0.2, 0.4], refSize= cellSize;
        } else if(topology.localeCompare('hexagonal')==0){
          var xlims= [-0.35, 0.35], ylims = [-0.25, -0.125, 0.0, 0.125, 0.25], refSize= innerCellSize;
        }
  
      	var axes = thePlot.append("g").selectAll(".cell")
  				.data(d3.range(nbRows * nbColumns))
  				.enter()
  				.append("path")
  				.attr("class", "axis")
  				.attr("d", function(d, i) {
            if (forceArray(forceArray(normalizedValues[i])[0])[0] == null) return null;
            if (isCatBarplot) if (cellPop[i] == 0) return null;
            var ch= ""
            for (var iy= 0; iy < ylims.length; iy++)
              ch = ch + "M " + (cellPositions[i].x + xlims[0] * refSize) + " , " + (cellPositions[i].y + ylims[iy] * refSize)+
  				          " L " + (cellPositions[i].x + xlims[1] * refSize) + " , " + (cellPositions[i].y + ylims[iy] * refSize);
    				  return ch;
  				})
          .attr("stroke", "#414141")
          .attr("stroke-opacity", "0.6")
          .attr("stroke-width", cellSize / 100);     
      } else if (plotType.localeCompare("Circular")==0) {
        var radii= [0.25, 0.5, 0.75, 1];
        for (var irad= 0; irad < radii.length; irad++) {
        	var axes = thePlot.append("g").selectAll(".cell")
    				.data(d3.range(nbRows * nbColumns))
    				.enter()
    				.append("circle")
    				.attr("class", "axis")
      			.attr("r", function(d, i) {
    				  if (forceArray(normalizedValues[i])[0] == null) return null;
    				  return radii[irad] * 0.95 * 0.4 * innerCellSize;
            })
      			.attr("transform", function(d, i) { 
    				  return 'translate(' + cellPositions[i].x + ',' + cellPositions[i].y + ')';
    				})
            .attr("stroke", "#414141")
            .attr("fill", "none")
            .attr("stroke-opacity", "0.6")
            .attr("stroke-width", cellSize / 100);     
        }
      } else if (plotType.localeCompare("Radar")==0) {
        var radii= [0.25, 0.5, 0.75, 1];
        for (var irad= 0; irad < radii.length; irad++) {
        	var axes = thePlot.append("g").selectAll(".cell")
    				.data(d3.range(nbRows * nbColumns))
    				.enter()
    				.append("path")
    				.attr("class", "axis")
    				.attr("d", function(d,i) {
    				  if (forceArray(normalizedValues[i])[0] == null) return null;
    				  var ch = " M" + (radii[irad] * 0.4 * innerCellSize * Math.cos(-0.5 * Math.PI)) + "," + (radii[irad] * 0.4 * innerCellSize * Math.sin(-0.5 * Math.PI));
    				  for (var j=1; j<nVars; j++) 
    				    ch+= " L" + (radii[irad] * 0.4 * innerCellSize * Math.cos(2 * Math.PI * (j / nVars - 0.25))) + "," + (radii[irad] * 0.4 * innerCellSize * Math.sin(2 * Math.PI * (j / nVars - 0.25)));
    				  return ch + " L" + (radii[irad] * 0.4 * innerCellSize * Math.cos(-0.5 * Math.PI)) + "," + (radii[irad] * 0.4 * innerCellSize * Math.sin(-0.5 * Math.PI));
    				})
      			.attr("transform", function(d, i) { 
    				  return 'translate(' + cellPositions[i].x + ',' + cellPositions[i].y + ')';
    				})
            .attr("stroke", "#414141")
            .attr("fill", "none")
            .attr("stroke-opacity", "0.6")
            .attr("stroke-width", cellSize / 100);     
        }
      	var axislines = thePlot.append("g").selectAll(".cell")
  				.data(d3.range(nbRows * nbColumns))
  				.enter()
  				.append("path")
  				.attr("class", "axisline")
  				.attr("d", function(d,i) {
  				  if (forceArray(normalizedValues[i])[0] == null) return null;
  				  var ch = "";
  				  for (var j=0; j<nVars; j++) 
  				    ch+= "M0,0 L" + (0.4 * innerCellSize * Math.cos(2 * Math.PI * (j / nVars - 0.25))) + "," + (0.4 * innerCellSize * Math.sin(2 * Math.PI * (j / nVars - 0.25)));
  				  return ch;
  				})
    			.attr("transform", function(d, i) { 
  				  return 'translate(' + cellPositions[i].x + ',' + cellPositions[i].y + ')';
  				})
          .attr("stroke", "#414141")
          .attr("fill", "none")
          .attr("stroke-opacity", "0.6")
          .attr("stroke-width", cellSize / 100);     
      }
    }

    /////////////////////////
    // Create legend (if appropriate)
    /////////////////////////
    if (legendPos != "none") if (plotType === "Circular" || plotType === "Barplot" || plotType === "Boxplot" || plotType === "Line" || plotType === "Pie" || plotType === "Color" || plotType === "Cloud") {
      
      var legendColors = legendReverse ? labelColor.slice().reverse():labelColor;

      // ColWidth: pixel size based on max nchar of the labels
      var legendColWidth = label.reduce(
        function (a, b) {return a.length > b.length ? a : b;}
      ).length * 0.6 * legendFontsize + 25;
      
      var theLegend= thePlot;
      var legendHeight, legendWidth, legendNcols, legendNrows, legendCentering;
      if (legendPos === "beside") { 
        // vertical layout (fixed height)
        legendNcols= 1 + Math.floor(label.length * (1.5 * legendFontsize) / height);
        legendNrows= Math.min(label.length, Math.floor(height / (1.5 * legendFontsize)));
        legendHeight= height;
        legendWidth= legendColWidth * legendNcols;
        legendCentering= (height - (legendNrows * (1.5 * legendFontsize))) / 2;
        theSvg.attr("height", height).attr("width", width + 2 * legendFontsize + legendWidth);
      } else { 
        // horizontal layout (fixed width)
        legendNcols= Math.min(label.length, Math.floor(width / legendColWidth));
        legendNrows= Math.ceil(label.length / legendNcols);
        legendNcols= Math.ceil(label.length / legendNrows);
        legendHeight= legendNrows * (1.5 * legendFontsize);
        legendWidth= width;
        legendCentering= (width - (legendNcols * legendColWidth)) / 2;
        theSvg.attr("height", height + legendFontsize + legendHeight).attr("width", width);
      }

      var legenddots = theLegend.selectAll("legenddots")
        .data(legendReverse ? label.slice().reverse():label).enter()
        .append("circle").attr("class", "legendElt")
        .attr("cx", function(d,i){ return (Math.floor(i/legendNrows) * legendColWidth + legendFontsize + (legendPos === "beside" ? 0:(legendFontsize + legendCentering)));})
        .attr("cy", function(d,i){ return i % legendNrows * (1.5 * legendFontsize) + (0.5 * legendFontsize) + (legendPos === "beside" ? legendCentering:0);})
        .attr("r", legendFontsize / 2)
        .style("fill", function(d,i){return legendColors[i];})
        .attr("transform", "translate(" + (legendPos === "beside" ? (legendFontsize + width):0) + " , " + (legendPos === "beside" ? 0:(legendFontsize + height)) + ")");
        
      var legendlabels = theLegend.selectAll("legendlabels")
        .data(legendReverse ? label.slice().reverse():label).enter()
        .append("text").attr("class", "legendElt")
        .attr("x", function(d,i){ return (Math.floor(i/legendNrows) * legendColWidth + 2 * legendFontsize + (legendPos === "beside" ? 0:(legendFontsize + legendCentering)));})
        .attr("y", function(d,i){ return i % legendNrows * (1.5 * legendFontsize) + (0.8 * legendFontsize) + (legendPos === "beside" ? legendCentering:0);})
        .style("fill", "black").attr("font-family", "arial").attr("font-size", legendFontsize)
        .text(function(d){ return d;})
        .attr("text-anchor", "left").style("alignment-baseline", "left")
        .attr("transform", "translate(" + (legendPos === "beside" ? (legendFontsize + width):0) + " , " + (legendPos === "beside" ? 0:(legendFontsize + height)) + ")");
        
        ///////////////////////
        // Legend interactivity
        if (plotType === "Circular") {
          thePlot.selectAll(".legendElt").on('mouseenter', function(m, d) {
            d3.select("#" + infoId).text("-");
            d3.select("#" + namesId).text("");
            d3.select("#" + messageId).text(d);
            thePlot.selectAll("path.piePart")
              .transition().duration(50)
              .attr("stroke", function(dd, di) {return label[dd.var] == d ? "white" : "none";})
    					.attr("opacity", function(dd, di) {
    					  if (!transparency) return 0.9; 
    					  return label[dd.var] == d ? 1 : 0.6;
    					})
              .attr("stroke-width",2 * cellSize / 100);
          })
          .on('mouseleave', function(m, d) {
            d3.select("#" + messageId).text("-");
            thePlot.selectAll("path.piePart")
              .transition().duration(50)
              .attr("opacity", 0.9)
              .attr("stroke","none");
          });
        } else if (plotType === "Barplot") {
          thePlot.selectAll(".legendElt")
          .on('mouseenter', function(m, d) {
            d3.select("#" + infoId).text("-");
            d3.select("#" + namesId).text("");
            d3.select("#" + messageId).text(d);
    				thePlot.selectAll("rect.bar")
    					.transition().duration(50)
    					.attr("stroke", function(bd, bi) {return label[bd.var]==d ? "white" : "none";})
    					.attr("stroke-width", function(bd, bi) {return label[bd.var]==d ? 2 * cellSize / 100 : null;})
    					.attr("opacity", function(bd, bi) {
    					  if (!transparency) return 0.9;
    					  return label[bd.var]==d ? 1 : 0.6;
    					});
      			})
    			.on('mouseleave', function(m, d) {
      		  d3.select("#" + messageId).text("-");
    				thePlot.selectAll("rect.bar")
    					.transition().duration(50)
    					.attr("stroke", "none")
    					.attr("opacity", 0.9);
    			});          
        } else if (plotType === "Boxplot") {
          thePlot.selectAll(".legendElt")
          .on('mouseenter', function(m, d) {
            d3.select("#" + infoId).text("-");
            d3.select("#" + namesId).text("");
            d3.select("#" + messageId).text(d);
    				thePlot.selectAll(".bp")
    					.transition().duration(50)
              .attr("opacity", function(bd, bi){
                if (!transparency) return 1;
                return label[bd.var] == d ? 1 : 0.5;
              });
    				thePlot.selectAll(".bpOutlier")
    					.transition().duration(50)
              .attr("opacity", function(bd, bi){
                if (!transparency) return 1;
                return label[bd.var] == d ? 1 : 0.5;
              });
    		  })
      		.on('mouseleave', function(m, d) {
      		  d3.select("#" + messageId).text("-");
    				thePlot.selectAll(".bp").transition().duration(50).attr("opacity", 1);
    				thePlot.selectAll(".bpOutlier").transition().duration(50).attr("opacity", 1);
      		});
        } else if (plotType === "Pie") {
          thePlot.selectAll(".legendElt")
            .on('mouseenter', function(m, d) {
              d3.select("#" + infoId).text("-");
              d3.select("#" + namesId).text("");
              d3.select("#" + messageId).text(d);
      				thePlot.selectAll("path.piePart")
      					.transition().duration(50)
      					.attr("stroke", function(dd) {return label[dd.var] == d ? "white" : "none";})
      					.attr("stroke-width", cellSize * 0.02);
      			})
            .on('mouseleave', function(m, d) {
        		  d3.select("#" + messageId).text("-");
      				thePlot.selectAll("path.piePart")
      				  .transition().duration(50).attr("stroke","none");
      			});
        } else if (plotType === "Line") {
          thePlot.selectAll(".legendElt")
            .on('mouseenter', function(m, d) {
              d3.select("#" + infoId).text("-");
              d3.select("#" + namesId).text("");
              d3.select("#" + messageId).text(d);
              var chosenPoint = label.indexOf(d);
      				thePlot.selectAll(".lineCircle")
        				.attr("cx", function(cd, ci) {return cd[chosenPoint].px;})
        				.attr("cy", function(cd, ci) {return cd[chosenPoint].py;});
            })
            .on('mouseleave', function(m, d) {
            });
       } else if (plotType == "Cloud") {
         thePlot.selectAll(".legendElt")
           .on('mouseenter', function(m, d) {
             d3.select("#" + infoId).text("-");
             d3.select("#" + namesId).text("");
             d3.select("#" + messageId).text(d);
    				 thePlot.selectAll(".cloudCircle")
    				   .attr("stroke", function(cd, ci) {
    				     return label[cd.color] === d ? "#111":(labelColor[cd.color]);
    				   })
    				   .attr("fill-opacity", function(cd, ci) {
                 if (!transparency) return 1;
    				     return label[cd.color] === d ? 1:0.1;
    				   })
    				   .attr("stroke-opacity", function(cd, ci) {
                 if (!transparency) return 1;
    				     return label[cd.color] === d ? 0.9:0.1;
    				   });
             })
           .on('mouseleave', function(m, d) {
             d3.select("#" + infoId).text("-");
             d3.select("#" + namesId).text("");
             d3.select("#" + messageId).text("-");
    				 thePlot.selectAll(".cloudCircle")
    				   .attr("stroke", function(cd, ci) {
    				     return labelColor[cd.color];
    				   })
    				   .attr("fill-opacity", transparency? 0.4:1)
    				   .attr("stroke-opacity", transparency? 0.8:1);
           });
      }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Cell plots
    ////////////////////////////////////////////////////////////////////////////
    
    if (plotType.localeCompare("Hitmap")==0) {        
      //////////////////////////////////////////////////////////////////////////
      // Hitmap (population map)
      //////////////////////////////////////////////////////////////////////////
  		var inner_shape  = thePlot.append("g")
  			.selectAll(".cell")
  			.data(cellPositions)
  			.enter()
  			.append("path")
  			.attr("class", "InnerShape")
  			.attr("d", function (d, i) {
          if(topology.localeCompare('rectangular')==0){
    				var innerSize = (normalizedValues[i]*innerCellSize);
    				return "M" + d.x + "," + d.y + 
              " m" + (-innerSize / 2) + "," + (innerSize / 2) +
              " l" + innerSize + ",0" + " l" + "0," + (-innerSize) + 
              " l" + (-innerSize) + ",0" + " l" + "0," + innerSize;
          } else if(topology.localeCompare('hexagonal')==0){
    				return "M" + d.x + "," + d.y + hexPath((0.5*normalizedValues[i]*innerCellSize));
          }
  			})
  			.style("fill", function (d,i) {return "#112E45";});

      inner_shape.on('mouseover', function(m, d) {
  			d3.select("#" + infoId).text('Cell ' + parseInt(d.cell+1,10) + ', Superclass ' +
            superclass[d.cell] + ', N= ' + cellPop[d.cell]);
  			d3.select("#" + namesId).text(cellNames[d.cell]);
				if (transparency) d3.select(this).transition().duration(10).style("fill-opacity", 0.8);
      })

      inner_shape.on('mouseout', function(m, d) {
        if (transparency) d3.select(this).transition().duration(400).style("fill-opacity", 1);
      })

    } else if (plotType.localeCompare("Color")==0) {
      ////////////////////////////////////////////////////////////////////////
      // Color plot
      ////////////////////////////////////////////////////////////////////////
      cells.style("fill", function (d,i) {
          return normalizedValues[i];
  			});

  		if(showSC){
  		  thePlot.append("g").selectAll(".cell")
    			.data(cellPositions).enter()
    			.append("text")
					.attr("x", function(d) { return d.x - innerCellSize*0.05 ; })
					.attr("y", function(d) { return d.y + innerCellSize*0.05 ; })
					.text(function(d, i) { return superclass[i]; })
					.attr("font-family", "sans-serif")
					.attr("font-size", innerCellSize*0.2)
					.attr("fill", "#112E45");
  		}

  		//hover effects for the hexagons to change opacity and get the text for the cell
  		cells.on('mouseover', function(m, d) {
  			if (transparency) d3.select(this).transition().duration(10).style("fill-opacity", 0.8);
  			d3.select("#" + infoId).text('Cell ' + parseInt(d.cell+1,10) + ', superclass ' +
            superclass[d.cell] + ', N= ' + cellPop[d.cell]);
  			d3.select("#" + messageId).text(forceArray(realValues[d.cell])[0] == null ? "-" : (colorVarName + " : " + realValues[d.cell]));
  			d3.select("#" + namesId).text(cellNames[d.cell]);
  		});
  		cells.on('mouseout', function(m, d, i) {
  			if (transparency) d3.select(this).transition().duration(400).style("fill-opacity", 1);
  		});
    } else if (plotType.localeCompare("Circular")==0) {
      //////////////////////////////////////////////////////////////////////////
      // Circular barplot
      //////////////////////////////////////////////////////////////////////////

      var barArray = [];
    	for (var iCell = 0; iCell < nbRows*nbColumns; iCell++) {
        for(var j=0; j<nVars; j++){
          barArray.push({norm: forceArray(normalizedValues[iCell])[j], 
                          real: forceArray(realValues[iCell])[j],
                          cellPop: cellPop[iCell],
                          var: j,
                          cell: iCell});
  			}
    	}
    	
      var pieParts = thePlot.append("g").selectAll(".pieParts")
  			.data(barArray).enter()
  			.append('path')
  			.attr("class", "piePart")
  			.attr("d", function(d, i) {
  			  return d3.arc()({innerRadius: 0, // d3 v6
  			  //return d3.svg.arc()({innerRadius: 0, // d3 v3
      			                   outerRadius: d.norm * 0.4 * innerCellSize,
      			                   startAngle: 2.0 * Math.PI * d.var / nVars,
      			                   endAngle: 2.0 * Math.PI * (d.var + 1) / nVars})
  			})
				.attr('transform', function(d, i) { 
				  return 'translate(' + cellPositions[d.cell].x + ',' + cellPositions[d.cell].y + ')';
				})
				.attr("fill", function(d, i) { return labelColor[d.var];})
				.attr("opacity", 0.9)
				.on('mouseenter', function(m, d) {
          d3.select("#" + messageId).text(label[d.var] + ": " + d.real);
          thePlot.selectAll("path.piePart")
            .transition().duration(50)
            .attr("stroke", function(dd, di) {return dd.var == d.var ? "white" : "none";})
  					.attr("opacity", function(dd, di) {
  					  if (!transparency) return 0.9; 
  					  return dd.var == d.var ? 1 : 0.6;
  					})
            .attr("stroke-width",2 * cellSize / 100);
        })
        .on('mouseleave', function(m, d) {
          d3.select("#" + messageId).text(function () {
            return "-";
          });
          thePlot.selectAll("path.piePart")
            .transition().duration(50)
            .attr("opacity", 0.9)
            .attr("stroke","none");
        });
        
    } else if(plotType.localeCompare("Barplot")==0) {
      //////////////////////////////////////////////////////////////////////////
      // Barplot
      //////////////////////////////////////////////////////////////////////////
      
      var barArray = [];
    	for (var iCell = 0; iCell < nbRows*nbColumns; iCell++) {
        for(var j=0; j<nVars; j++){
          barArray.push({norm: forceArray(normalizedValues[iCell])[j], 
                          real: forceArray(realValues[iCell])[j],
                          cellPop: cellPop[iCell],
                          var: j,
                          cell: iCell});
  			}
    	}
    	
      if(topology.localeCompare('rectangular')==0){
        var widthMult = 0.8, heightMult = 0.9, refCellSize= cellSize;
      } else if(topology.localeCompare('hexagonal')==0){
        var widthMult = 0.7, heightMult = 0.55, refCellSize= innerCellSize;
      }
      
			var bars = thePlot.append("g").selectAll(".bars")
				.data(barArray).enter()
				.append("rect")
				.attr("class", "bar")
				.attr("opacity", 0.9)
				.attr("x", function (d, i) {
					return refCellSize * (-widthMult/2 + d.var*widthMult/nVars + 0.01);
				})
				.attr("y", function (d, i) { 
				  return refCellSize * heightMult * (0.5 - d.norm); 
				})
				.attr("width", function (d) { return refCellSize * (widthMult/nVars - 0.02) })
				.attr("height", function (d, i) { return d.norm * refCellSize * heightMult; })
				.attr('transform', function(d, i) { return 'translate(' + cellPositions[d.cell].x + ',' + cellPositions[d.cell].y + ')';})
				.attr("fill", function(d, i) { return labelColor[d.var];})
  			.on('mouseenter', function(m, d) { // d3 v6.3.1
  			//.on('mouseenter', function(d, i) { // d3 v3.5.14
  				d3.select("#" + messageId).text(function () {
  					var ch= label[d.var] + ": " + d.real;
  					if (isCatBarplot)
  					  ch= ch + " (" + (100 * d.real / d.cellPop).toFixed(1) + "%)";
  					return ch;
  				});
  				thePlot.selectAll("rect.bar")
  					.transition().duration(50)
  					.attr("stroke", function(bd, bi) {return bd.var==d.var ? "white" : "none";})
  					.attr("stroke-width", function(bd, bi) {return bd.var==d.var ? 2 * cellSize / 100 : null;})
  					.attr("opacity", function(bd, bi) {
  					  if (!transparency) return 0.9;
  					  return bd.var==d.var ? 1 : 0.6;
  					});
    			})
  			.on('mouseleave', function(m, d) {
          d3.select("#" + messageId).text("-");
  				thePlot.selectAll("rect.bar")
  					.transition().duration(50)
  					.attr("stroke", "none")
  					.attr("opacity", 0.9);
  			});

    } else if(plotType.localeCompare("Radar")==0) {
      //////////////////////////////////////////////////////////////////////////
      // Radar plot
      //////////////////////////////////////////////////////////////////////////

      var barArray = [], polyArray = [];
    	for (var iCell = 0; iCell < nbRows*nbColumns; iCell++) {
    	  polyArray[iCell]= [];
        for(var j=0; j<nVars; j++){
          var dax = forceArray(normalizedValues[iCell])[j] * 0.4 * innerCellSize * Math.cos(2 * Math.PI * (j / nVars - 0.25)), 
            day = forceArray(normalizedValues[iCell])[j] * 0.4 * innerCellSize * Math.sin(2 * Math.PI * (j / nVars - 0.25));
          barArray.push({x: dax, y: day,
                         real: forceArray(realValues[iCell])[j],
                         cellPop: cellPop[iCell],
                         var: j, cell: iCell});
          polyArray[iCell].push({x: dax, y: day});
  			}
    	}
    	
			var radarPols = thePlot.append("g").selectAll(".radarPols")
				.data(polyArray).enter()
				.append("path")
				.attr("class", "radarPol")
				.attr("d", function (d, i) {
				  var ch = " M" + d[0].x + "," + d[0].y;
				  for (var j=1; j<nVars; j++) ch+= " L" + d[j].x + "," + d[j].y;
				  return ch + " L" + d[0].x + "," + d[0].y;
				})
				.attr('transform', function(d, i) { return 'translate(' + cellPositions[i].x + ',' + cellPositions[i].y + ')';})
				.attr("fill-opacity", 0.5)
				.attr("fill", "#77ADD9")
        .attr("stroke", "#112E45")
        .attr("stroke-width", cellSize * 0.02);

			var radarPoints = thePlot.append("g").selectAll(".radarPoints")
				.data(barArray).enter()
				.append("circle")
				.attr("class", "radarPoint")
				.attr("cx", function(d,i) {return d.x;})
				.attr("cy", function(d,i) {return d.y;})
				.attr("r", function(d,i) { return d.real == null ? null : cellSize * 0.04;})
				.attr("opacity", 0.9)
				.attr("fill", "#112E45")
				.attr('transform', function(d, i) { return 'translate(' + cellPositions[d.cell].x + ',' + cellPositions[d.cell].y + ')';})
				.on('mouseover', function(m, d){
      		d3.select("#" + messageId).text(label[d.var] + ': ' + d.real);
      		d3.selectAll(".radarPoint")
  			  .attr("opacity", function(cd) {
  			    if (!transparency) return 0.9;
  			    return cd.var == d.var ? 1 : 0.5;
  			  });
    	  })
      	.on('mouseout', function(m, d){
      		d3.select("#" + messageId).text("-");
      		d3.selectAll(".radarPoint").attr("opacity", 0.9);
        });
        
    } else if(plotType.localeCompare("Line")==0) {
      //////////////////////////////////////////////////////////////////////////
      // Line plot
      //////////////////////////////////////////////////////////////////////////
      
      if(topology.localeCompare('rectangular')==0){
        var xOffset= 0.1, xMult= 0.8, yOffset= 0.2, yMult= 0.9, refSize= cellSize;
      } else if(topology.localeCompare('hexagonal')==0){
        var xOffset= 0.2, xMult= 0.6, yOffset= 0.0, yMult= 0.5, refSize= innerCellSize;
      }
      
      var lineArray = [];
    	for (var iCell = 0; iCell < nbRows*nbColumns; iCell++) {
    	  lineArray[iCell]= [];
    	  var nullCell = normalizedValues[iCell][0] == null;
  			for(var j=0; j<nVars; j++){
      	  lineArray[iCell][j] = [];
  				lineArray[iCell][j].px = nullCell ? null : refSize * (xOffset + j*xMult/(nVars-1));
  				lineArray[iCell][j].py = nullCell ? null : cellSize * (yOffset - yMult * normalizedValues[iCell][j]);
  			}
    	}
    	
			var lines = thePlot.append("g").selectAll(".cell")
				.data(lineArray)
				.enter()
				.append("path")
				.attr("class", "ligne")
				.attr("d", function(d, i) {
				  if (d[0].px == null) return "";
				  var ch= "M" + d[0].px + "," + d[0].py;
				  for (var j= 0; j<nVars; j++) 
				   ch= ch + " L" + d[j].px + "," + d[j].py;
				  return ch;
				})
				.attr('transform', function(d, i) {
				  return 'translate(' + (cellPositions[i].x-innerCellSize/2) + ',' + (cellPositions[i].y+innerCellSize/4) + ')';
				})
				.attr("stroke", "#112E45")
				.attr("stroke-width", 1.5 * innerCellSize / 100)
				.attr("fill", "none");

			var circles = lines.select("path.ligne")
				.data(lineArray)
				.enter()
				.append("circle")
				.attr("class", "lineCircle")
				.attr("cx", function(d, i) {
					return lineArray[i][0].px;
				})
				.attr("cy", function(d, i) {
					return lineArray[i][0].py;
				})
				.attr('transform', function(d, i) {
				  return 'translate(' + (cellPositions[i].x-innerCellSize/2) + ',' + (cellPositions[i].y+innerCellSize*25/100) + ')';
				})
				.attr("r", 4 * innerCellSize / 100)
				.style("fill", "none")
				.attr("stroke", function(d, i) {
				  if (lineArray[i][0].px == null) return "none";
				  return "#112E45";
				})
				.attr("stroke-width", innerCellSize / 100);

			cells.on('mousemove', function(m, d) { 
  			//var mouseX = d3.mouse(this)[0]; // d3 v3
  			// Get mouse X, with trick for different browser treatments of m.offsetX
		    if (m.which == 0) { // m.which is 0 when m.offsetX is not cell-based
  			  if (topology.localeCompare('rectangular')==0) {
  			    var mouseX = (m.offsetX - 1) % Math.round(cellSize);
  			  } else {
			      var mouseX = ((m.offsetX - 1) + Math.round(((d.row + 1) % 2) *  hexRadius * Math.sqrt(3) * 0.5))  % Math.round(hexRadius * Math.sqrt(3));
  			  }
		    } else {
			      var mouseX = m.offsetX;
		    }
		    
		    // Get chosen point
  			var chosenPoint = 0;
  			for (var j=0; j<nVars; j++){
  			  if (topology.localeCompare('rectangular')==0) {
    			  var mouseBoundary = cellSize * (0.1 + (j+0.5) *0.8/(nVars-1));
  			  } else if (topology.localeCompare('hexagonal')==0) {
    			  var mouseBoundary = hexRadius * Math.sqrt(3) * (0.2 + (j+0.5) *0.6/(nVars-1));
  			  }
  			  if (mouseX > mouseBoundary) {
  			    chosenPoint= j + 1;
  			  }
  			}
  			chosenPoint= chosenPoint == nVars ? chosenPoint-1 : chosenPoint;
  			
				thePlot.selectAll(".lineCircle")
  				.attr("cx", function(cd, ci) {return lineArray[ci][chosenPoint].px;})
  				.attr("cy", function(cd, ci) {return lineArray[ci][chosenPoint].py;});

			  d3.select("#" + messageId).text(label[chosenPoint] + ': ' + 
			      (realValues[d.cell][chosenPoint]==null ? "-" : realValues[d.cell][chosenPoint]));
				
			});

    } else if(plotType.localeCompare("Boxplot")==0) {
      //////////////////////////////////////////////////////////////////////////
      // Boxplot
      //////////////////////////////////////////////////////////////////////////

		  if (topology.localeCompare('rectangular')==0) {
      	var widthMult = 0.8, heightMult = 0.8, refSize= cellSize;
		  } else if (topology.localeCompare('hexagonal')==0) {
      	var widthMult = 0.7, heightMult = 0.5, refSize= innerCellSize;
		  }
      var boxWidth = (refSize*widthMult)/(nVars),
    		boxHeight = (refSize*heightMult);
		  
  		for (var theCell = 0; theCell < nbRows*nbColumns; theCell++) {
  		  if (normalizedValues[theCell][0] == null) {continue;}
  		  
  		  var boxArray = [];
  		  for (var theVar = 0; theVar < nVars; theVar++) {
  		    boxArray[theVar] = [];
  		    boxArray[theVar].norm = normalizedValues[theCell][theVar];
  		    boxArray[theVar].real = realValues[theCell][theVar];
  		    boxArray[theVar].cell = theCell;
  		    boxArray[theVar].var = theVar;
  		  }
  		  
  		  var bpBoxes = thePlot.append("g").selectAll(".bpBox")
  				.data(boxArray)
  				.enter().append("rect")
  				.attr("y", function(d,i) {return (- d.norm[3] * boxHeight) ;})
  				.attr("class", "bp")
  				.attr("width",  boxWidth * 0.8)
  				.attr("height", function(d, i) {return (d.norm[3] - d.norm[1])*boxHeight;})
  				.attr('transform', function(d, i) {
						return 'translate(' + (cellPositions[theCell].x + 0.1 * boxWidth + (i / nVars - 0.5) * widthMult * refSize) + ',' + (cellPositions[theCell].y + boxHeight * (0.5)) + ')';
  				})
  				.attr('fill', function(d, i) {
  						return labelColor[i];
  				})
          .attr("stroke", "#000")
          .attr("stroke-width", cellSize * 0.005);
          
  			for (theVar = 0; theVar < nVars; theVar++) {
  			  if (normalizedExtremesValues[theCell][theVar].length == 0) continue;
  			  normalizedExtremesValues[theCell][theVar] = forceArray(normalizedExtremesValues[theCell][theVar]);
  			  realExtremesValues[theCell][theVar] = forceArray(realExtremesValues[theCell][theVar]);
  			  var boxOutArray = [];
  			  for (var iOut = 0; iOut < normalizedExtremesValues[theCell][theVar].length; iOut++) {
  			    boxOutArray[iOut]= [];
  			    boxOutArray[iOut].norm = normalizedExtremesValues[theCell][theVar][iOut];
  			    boxOutArray[iOut].real = realExtremesValues[theCell][theVar][iOut];
  			    boxOutArray[iOut].cell = theCell;
  			    boxOutArray[iOut].var = theVar;
  			  }

    		  var bpOutliers = thePlot.append("g").selectAll(".bpOutlier")
    		    .data(boxOutArray).enter()
    		    .append("circle")
    		    .attr("class", "bpOutlier")
    		    .attr("fill", labelColor[theVar])
    		    .attr("r", cellSize * 0.03)
    		    .attr("stroke", "#000")
    				.attr("cx", boxWidth * 0.4)
    				.attr("cy", function(d,i) {return -d.norm * boxHeight;})
            .attr('transform', 'translate(' + (cellPositions[theCell].x + 0.1 * boxWidth + (theVar / nVars - 0.5) * widthMult * refSize) + ',' + (cellPositions[theCell].y + boxHeight * (0.5)) + ')');

  			}
  		  
  		  // Lines and Whiskers
  		  var bpLines = thePlot.append("g").selectAll(".bpLine")
  				.data(boxArray)
  				.enter().append("path")
          .attr("class", "bp")
          .attr("stroke", "#000")
          .attr("stroke-width", cellSize * 0.007)
          .attr("d", function(d,i) {
            if (d.norm[0] == null) return null;
            return " M" + "0," + (-d.norm[0] * boxHeight) + " l" + (boxWidth * 0.8) + ",0" + 
                   " M" + "0," + (-d.norm[2] * boxHeight) + " l" + (boxWidth * 0.8) + ",0" + 
                   " M" + "0," + (-d.norm[4] * boxHeight) + " l" + (boxWidth * 0.8) + ",0" + 
                   " M" + (boxWidth * 0.4) + "," + (-d.norm[0] * boxHeight) + " L" + (boxWidth * 0.4) + "," + (-d.norm[1] * boxHeight) +
                   " M" + (boxWidth * 0.4) + "," + (-d.norm[3] * boxHeight) + " L" + (boxWidth * 0.4) + "," + (-d.norm[4] * boxHeight);
          })
          .attr('transform', function(d, i) {
						return 'translate(' + (cellPositions[theCell].x + 0.1 * boxWidth + (i / nVars - 0.5) * widthMult * refSize) + ',' + (cellPositions[theCell].y + boxHeight * (0.5)) + ')';
  				});
  				
  		}
  		
			// Boxes and lines mouse events
  		thePlot.selectAll(".bp").on('mouseenter', function(m, d) {
				d3.select("#" + messageId).text(function () {
					var ch=label[d.var]+ ": Q1= " + d.real[1] + " ; Med= " +
						  d.real[2] + " ; Q3= " + d.real[3];
					return ch;
				});
				thePlot.selectAll(".bp")
					.transition().duration(50)
          .attr("opacity", function(bd, bi){
            if (!transparency) return 1;
            return bd.var == d.var ? 1 : 0.5;
          });
				thePlot.selectAll(".bpOutlier")
					.transition().duration(50)
          .attr("opacity", function(bd, bi){
            if (!transparency) return 1;
            return bd.var == d.var ? 1 : 0.5;
          });
  		});
  		thePlot.selectAll(".bp").on('mouseleave', function(m, d) {
  		  d3.select("#" + messageId).text("-");
				thePlot.selectAll(".bp").transition().duration(50).attr("opacity", 1);
				thePlot.selectAll(".bpOutlier").transition().duration(50).attr("opacity", 1);
  		});
  		
			// Outliers mouse events
  		thePlot.selectAll(".bpOutlier").on('mouseenter', function(m, d) {
				d3.select("#" + messageId).text(label[d.var]+ ": " + d.real);
				thePlot.selectAll(".bp")
					.transition().duration(50)
          .attr("opacity", function(bd, bi){
            if (!transparency) return 1;
            return bd.var == d.var ? 1 : 0.5;
          });
				thePlot.selectAll(".bpOutlier")
					.transition().duration(50)
					.attr("opacity", function(bd, bi){
					  if (!transparency) return 1;
            return bd.var == d.var ? 1 : 0.5;
          });
  		});
  		thePlot.selectAll(".bpOutlier").on('mouseleave', function(m, d) {
  		  d3.select("#" + messageId).text("-");
				thePlot.selectAll(".bp").transition().duration(50).attr("opacity", 1);
				thePlot.selectAll(".bpOutlier").transition().duration(50).attr("opacity", 1);
  		});

    } else if(plotType.localeCompare("Pie")==0) {
      //////////////////////////////////////////////////////////////////////////
      // Pie plot
      //////////////////////////////////////////////////////////////////////////
      if (topology.localeCompare("rectangular")==0) {
  			var sizeMult= 0.5;
      } else if (topology.localeCompare("hexagonal")==0) {
  			var sizeMult= 0.4;
      }
        
      var pieArray = [];
    	for (var iCell = 0; iCell < nbRows*nbColumns; iCell++) {
        var startAng= 0, endAng= 0;
        for(var j=0; j<nVars; j++){
          var tmpVal = forceArray(normalizedValues[iCell])[j];
          if (tmpVal == 0.0) continue;
          endAng+= tmpVal;
          pieArray.push({startAng: startAng, endAng: endAng,
                          real: forceArray(realValues[iCell])[j],
                          size: normalizedSize[iCell],
                          cellPop: cellPop[iCell],
                          var: j, cell: iCell});
    			startAng+= tmpVal;
  			}
    	}

			var pieParts = thePlot.append("g").selectAll(".pieParts")
				.data(pieArray).enter()
				.append('path')
        .attr("class", "piePart")
				.attr('d', function(d, i) {
				  var startAng= 0, endAng= 0;
  			  return d3.arc()({innerRadius: 0, // d3 v6
  			  //return d3.svg.arc()({innerRadius: 0, // d3 v3
      			                   outerRadius: d.size * sizeMult * innerCellSize,
      			                   startAngle: 2.0 * Math.PI * d.startAng,
      			                   endAngle: 2.0 * Math.PI * d.endAng});
				})
				.attr('transform', function(d,i) {
				  return 'translate(' + (cellPositions[d.cell].x) +',' + cellPositions[d.cell].y + ')';
				})
				.attr('fill', function(d, i) {return labelColor[d.var];})
				.on('mouseenter', function(m, d) {
  				d3.select("#" + messageId).text(label[d.var]+ ": " + d.real + " (" +
  					  (100 * d.real / d.cellPop).toFixed(1) + "%)");
  				thePlot.selectAll("path.piePart")
  					.transition().duration(50)
  					.attr("stroke", function(dd) {return dd.var == d.var ? "white" : "none";})
  					.attr("stroke-width", cellSize * 0.02);
  			})
        .on('mouseleave', function(m, d) {
          d3.select("#" + messageId).text("-");
  				thePlot.selectAll("path.piePart")
  				  .transition().duration(50).attr("stroke","none");
  			});
    } else if(plotType.localeCompare("Cloud")==0) {
      //////////////////////////////////////////////////////////////////////////
      // Cloud plot
      //////////////////////////////////////////////////////////////////////////
      var cloudArray = [];
      var cloudTooltip =  d3.select("#" + plotId)
        .append("div").attr("class", "cloudTooltip").attr("id", "cloudTooltip")
        .style("position", "fixed")
        .style("z-index", "10")
        .style("opacity", 0)
        .style("font-family", "arial")
        .style("font-size", legendFontsize + "px")
        .style("background-color", "white")
        .style("border", "solid")
        .style("border-width", "2px")
        .style("border-radius", "5px")
        .style("padding", "5px");
      var ctElt = document.getElementById("cloudTooltip");
        
      var sizeMult = (topology == "rectangular" ? 0.7:0.55)
      for (var iObs = 0; iObs < normalizedValues.length; iObs++) {
        cloudArray.push({x: normalizedValues[iObs][0] * sizeMult * innerCellSize, 
                         y: normalizedValues[iObs][1] * sizeMult * innerCellSize, 
                         obs: iObs,
                         cell: clustering[iObs] - 1, 
                         name: realValues[iObs], 
                         color: cloudColor[iObs]});
      }
			var cloudPoints = thePlot.append("g").selectAll(".cloudPoints")
				.data(cloudArray).enter()
				.append('circle').attr("class", "cloudCircle")
				.attr("r", cellSize * 0.05)
				.attr("fill", function(d) {return(labelColor[d.color])})
				.attr("stroke", function(d) {return(labelColor[d.color])})
				.attr("fill-opacity", transparency? 0.3:1)
		    .attr("stroke-opacity", transparency? 0.8:1)
		    .attr("stroke-width", cellSize * 0.02)    
				.attr("cx", function(d) {return(d.x);})
				.attr("cy", function(d) {return(d.y);})
				.attr('transform', function(d,i) {
				  return 'translate(' + (cellPositions[d.cell].x) +',' + cellPositions[d.cell].y + ')';
				})
				.on("mouseenter", function(m, d) {
  				d3.select("#" + messageId)
  				  .text(d.name + (label.length > 1 ? (" : " + label[d.color]):""));

  				d3.selectAll(".cloudCircle")
  				  .attr("fill-opacity", function(dd) {
  				    return transparency ? (dd.obs === d.obs ? 1:0.4):1;
  				  })
  				  .attr("stroke", function(dd) {
  				    return dd.obs === d.obs ? "#111":labelColor[dd.color];
  				  });
  				  
  				if (fullData.length > 0) {
            var thetitle = "<strong>" + d.name + "</strong>" + "<table>";
            for (var iVar= 0; iVar < fullData[0].length; iVar++) {
              thetitle += "<tr> <td> <strong>" + forceArray(fullDataNames)[iVar] + "</strong> &nbsp;</td><td>" + fullData[d.obs][iVar] + "</td></tr>";
            }
            thetitle += "</table>";
            cloudTooltip.style("opacity", 1).html(thetitle);
            // Adaptive placement for tooltip
            if ((cellPositions[d.cell].col + 1) > (nbColumns / 2)) {
              cloudTooltip.style("left", (m.clientX - cellSize / 4 - ctElt.clientWidth) + "px");
            } else {
              cloudTooltip.style("left", (m.clientX + cellSize / 4) + "px");
            }
            // Ensure that the tooltip is not cut by top or bottom of window
            var theRow = (topology === "hexagonal" ? (cellPositions[d.cell].row - 1):(nbRows - 1 - cellPositions[d.cell].row));
            var ctTop = m.clientY - ((theRow + 1) > (nbRows / 2) ? ctElt.clientHeight:0);
            if (ctTop + ctElt.clientHeight > window.innerHeight)
              ctTop = window.innerHeight - ctElt.clientHeight;
            if (ctTop < 0) ctTop = 0;
            cloudTooltip.style("top", ctTop + "px");
  				}
				})
				.on("mouseleave", function(m, d) {
  				cloudTooltip.style("opacity", 0).html("");
				  d3.select("#" + messageId).text("-");
  				d3.selectAll(".cloudCircle")
						.attr("fill-opacity", transparency? 0.4:1)
  				  .attr("stroke", function(dd) {
  				    return labelColor[dd.color];
  				  });
				});
    }
  }, 
         resize: function(width, height) {}
    };
  }
});
