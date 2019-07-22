var fanchart = document.getElementById( "fanchart" );
var places_list = document.getElementById( "places_list" );

function pos_x( r, a ) {
	return center_x + r * Math.cos( Math.PI / 180 * a );
}
function pos_y( r, a ) {
	return center_y + r * Math.sin( Math.PI / 180 * a );
}
function up( r, a1, a2, sosa, p ) {
	l = path1( "tpiS"+sosa, r, a1, a2 );
	link( "tpiS"+sosa, p, l );
}
function no_up( r, a1, a2, sosa, p ) {
	l = path1( "tpiS"+sosa, r, a1, a2 );
	no_link( "tpiS"+sosa, p, l );
}
function pie_bg( id, r1, r2, a1, a2, p ) {
	var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
	path.setAttribute( "d",
		 'M ' + pos_x(r2, a1) + ',' + pos_y(r2, a1) +
		' A ' + r2 + ' ' + r2 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 1 ' + pos_x(r2, a2) + ',' + pos_y(r2, a2) +
		' L ' + pos_x(r1, a2) + ',' + pos_y(r1, a2) +
		' A ' + r1 + ' ' + r1 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 0 ' + pos_x(r1, a1) + ',' + pos_y(r1, a1) +
		' Z'
	);
	path.setAttribute( "id", id );
	var c = "";
	if( p.birth_place !== undefined && p.birth_place != "" ) {
		c += " "+lieux[p.birth_place].c;
	}
	if( p.death_place !== undefined && p.death_place != "" ) {
		c += " "+lieux[p.death_place].c;
	}
	if( p.death_age !== undefined && p.death_age != "" ) {
		c += " DA"+(Math.trunc(p.death_age/10)*10);
	}
	path.setAttribute( "class", c );
	fanchart.append(path);
}
function pie( id, r1, r2, a1, a2, p ) {
	var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
	path.setAttribute( "d",
		 'M ' + pos_x(r2, a1) + ',' + pos_y(r2, a1) +
		' A ' + r2 + ' ' + r2 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 1 ' + pos_x(r2, a2) + ',' + pos_y(r2, a2) +
		' L ' + pos_x(r1, a2) + ',' + pos_y(r1, a2) +
		' A ' + r1 + ' ' + r1 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 0 ' + pos_x(r1, a1) + ',' + pos_y(r1, a1) +
		' Z'
	);
	path.setAttribute( "class", "link" );
	fanchart.append(path);
	path.onclick = function() {
		var oc = p.oc;
		if( oc != "" && oc != 0 ) { oc = "&oc=" + oc } else { oc = "" }
		window.location = link_to_person + "p=" + p.fnk + "&n=" + p.snk + oc
	};
	path.onmouseenter = function() {
		if( p.birth_place !== undefined && p.birth_place != "" ) {
			document.getElementById( "bi-" + lieux[p.birth_place].c ).classList.remove("hidden");
		}
		if( p.death_place !== undefined && p.death_place != "" ) {
			document.getElementById( "de-" + lieux[p.death_place].c ).classList.remove("hidden");
		}
		if( p.marriage_place !== undefined && p.marriage_place != "" ) {
			document.getElementById( "ma-" + lieux[p.marriage_place].c ).classList.remove("hidden");
		}
		if( p.death_age !== undefined && p.death_age != "" ) {
			var c = "DA"+(Math.trunc(p.death_age/10)*10);
			document.getElementById( c ).classList.add("hl_b");
		}
	};
	path.onmouseleave = function() {
		if( p.birth_place !== undefined && p.birth_place != "" ) {
			document.getElementById( "bi-" + lieux[p.birth_place].c ).classList.add("hidden");
		}
		if( p.death_place !== undefined && p.death_place != "" ) {
			document.getElementById( "de-" + lieux[p.death_place].c ).classList.add("hidden");
		}
		if( p.marriage_place !== undefined && p.marriage_place != "" ) {
			document.getElementById( "ma-" + lieux[p.marriage_place].c ).classList.add("hidden");
		}
		if( p.death_age !== undefined && p.death_age != "" ) {
			var c = "DA"+(Math.trunc(p.death_age/10)*10);
			document.getElementById( c ).classList.remove("hl_b");
		}
	};

	if( p.fn == "=" ) {
		path.addEventListener( "mouseenter", function() {
			var ref = document.getElementById( "S"+p.sn );
			ref.classList.add( "highlight" );
		});
		path.addEventListener( "mouseout", function() {
			var ref = document.getElementById( "S"+p.sn );
			ref.classList.remove( "highlight" );
		});
	}
}
function pie_m_bg( id, r1, r2, a1, a2, p ) {
	var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
	path.setAttribute( "d",
		 'M ' + pos_x(r2, a1) + ',' + pos_y(r2, a1) +
		' A ' + r2 + ' ' + r2 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 1 ' + pos_x(r2, a2) + ',' + pos_y(r2, a2) +
		' L ' + pos_x(r1, a2) + ',' + pos_y(r1, a2) +
		' A ' + r1 + ' ' + r1 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 0 ' + pos_x(r1, a1) + ',' + pos_y(r1, a1) +
		' Z'
	);
	path.setAttribute( "id", id );
	var c = "";
	if( p.marriage_place !== undefined && p.marriage_place != "" ) {
		c += " "+lieux[p.marriage_place].c;
	}
	path.setAttribute( "class", c );
	fanchart.append(path);
}
function pie_m( id, r1, r2, a1, a2, p ) {
	var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
	path.setAttribute( "d",
		 'M ' + pos_x(r2, a1) + ',' + pos_y(r2, a1) +
		' A ' + r2 + ' ' + r2 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 1 ' + pos_x(r2, a2) + ',' + pos_y(r2, a2) +
		' L ' + pos_x(r1, a2) + ',' + pos_y(r1, a2) +
		' A ' + r1 + ' ' + r1 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 0 ' + pos_x(r1, a1) + ',' + pos_y(r1, a1) +
		' Z'
	);
	fanchart.append(path);
	path.onmouseenter = function() {
		if( p.marriage_place !== undefined && p.marriage_place != "" ) {
			document.getElementById( "ma-" + lieux[p.marriage_place].c ).classList.remove("hidden");
		}
	};
	path.onmouseleave = function() {
		if( p.marriage_place !== undefined && p.marriage_place != "" ) {
			document.getElementById( "ma-" + lieux[p.marriage_place].c ).classList.add("hidden");
		}
	};
}
function pie_contour( r1, r2, a1, a2 ) {
	var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
	path.setAttribute( "d",
		 'M ' + pos_x(r2, a1) + ',' + pos_y(r2, a1) +
		' A ' + r2 + ' ' + r2 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 1 ' + pos_x(r2, a2) + ',' + pos_y(r2, a2) +
		' L ' + pos_x(r1, a2) + ',' + pos_y(r1, a2) +
		' A ' + r1 + ' ' + r1 + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 0 ' + pos_x(r1, a1) + ',' + pos_y(r1, a1) +
		' Z'
	);
	path.setAttribute( "class", "contour" );
	fanchart.append(path);
}
function pie_middle( r1, r2, a ) {
	var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
	path.setAttribute( "d",
		 'M ' + pos_x(r2, a) + ',' + pos_y(r2, a) +
		' L ' + pos_x(r1, a) + ',' + pos_y(r1, a)
	);
	path.setAttribute( "class", "middle" );
	fanchart.append(path);
}
function circle_bg( id, r, cx, cy, p ) {
	var circle = document.createElementNS("http://www.w3.org/2000/svg", "circle");
	circle.setAttribute( "cx", cx );
	circle.setAttribute( "cy", cy );
	circle.setAttribute( "r", r );
	circle.setAttribute( "id", id );
	var c = "";
	if( p.birth_place !== undefined && p.birth_place != "" ) {
		c += " "+lieux[p.birth_place].c;
	}
	if( p.death_place !== undefined && p.death_place != "" ) {
		c += " "+lieux[p.death_place].c;
	}
	if( p.death_age !== undefined && p.death_age != "" ) {
		c += " DA"+(Math.trunc(p.death_age/10)*10);
	}
	circle.setAttribute( "class", c );
	fanchart.append(circle);
}
function circle( id, r, cx, cy, p ) {
	var circle = document.createElementNS("http://www.w3.org/2000/svg", "circle");
	circle.setAttribute( "cx", cx );
	circle.setAttribute( "cy", cy );
	circle.setAttribute( "r", r );
	circle.setAttribute( "class", "link" );
	fanchart.append(circle);
	circle.onclick = function() {
		var oc = p.oc;
		if( oc != "" && oc != 0 ) { oc = "&oc=" + oc } else { oc = "" }
		window.location = link_to_person + "p=" + p.fnk + "&n=" + p.snk + oc
	};
	circle.onmouseenter = function() {
		if( p.birth_place !== undefined && p.birth_place != "" ) {
			document.getElementById( "bi-" + lieux[p.birth_place].c ).classList.remove("hidden");
		}
		if( p.death_place !== undefined && p.death_place != "" ) {
			document.getElementById( "de-" + lieux[p.death_place].c ).classList.remove("hidden");
		}
		if( p.death_age !== undefined && p.death_age != "" ) {
			var c = "DA"+(Math.trunc(p.death_age/10)*10);
			document.getElementById( c ).classList.add("hl_b");
		}
	};
	circle.onmouseleave = function() {
		if( p.birth_place !== undefined && p.birth_place != "" ) {
			document.getElementById( "bi-" + lieux[p.birth_place].c ).classList.add("hidden");
		}
		if( p.death_place !== undefined && p.death_place != "" ) {
			document.getElementById( "de-" + lieux[p.death_place].c ).classList.add("hidden");
		}
		if( p.death_age !== undefined && p.death_age != "" ) {
			var c = "DA"+(Math.trunc(p.death_age/10)*10);
			document.getElementById( c ).classList.remove("hl_b");
		}
	};
}
function text_S1( x, y, p ) {
	var text = document.createElementNS("http://www.w3.org/2000/svg", "text");
	text.setAttribute( "x", x );
	text.setAttribute( "y", y );
	text.setAttribute( "class", "gen1" );
	var ts1 = 100;
        standard.textContent = p.fn;
	if( standard.getBBox().width > 2*a_r[0]*security ) {
		ts1 = Math.round( 100 * 2*a_r[0]*security / standard.getBBox().width );
	}
	var ts2 = 100;
        standard.textContent = p.sn;
	if( standard.getBBox().width > 2*a_r[0]*security ) {
		ts2 = Math.round( 100 * 2*a_r[0]*security / standard.getBBox().width );
	}
	text.innerHTML = '<tspan style="font-size:'+ts1+'%">' + p.fn + '</tspan><tspan x="' + x + '" dy="15" style="font-size:'+ts2+'%">' + p.sn + '</tspan><tspan class="dates" x="' + x + '" dy="15">' + p.dates + '</tspan>';
	fanchart.append(text);
}

function path1( id, r, a1, a2 ) {
	var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
	path.setAttribute( "class", "none" );
	path.setAttribute( "d",
		 'M ' + pos_x(r, a1) + ',' + pos_y(r, a1) +
		' A ' + r + ' ' + r + ' 0 ' + (a2 - a1 > 180 ? 1 : 0) + ' 1 ' + pos_x(r, a2) + ',' + pos_y(r, a2)
	);
	path.setAttribute( "id", id );
	fanchart.append(path);

	return Math.abs(a2-a1)/360*2*Math.PI*r;
}
function path2( id, r1, r2, a ) {
	var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
	path.setAttribute( "class", "none" );
	path.setAttribute( "d",
		 'M ' + pos_x(r1, a) + ',' + pos_y(r1, a) +
		' L ' + pos_x(r2, a) + ',' + pos_y(r2, a)
	);
	path.setAttribute( "id", id );
	fanchart.append(path);

	return Math.abs(r2-r1);
}
function text2( pid, t, c, l, h ) {
        standard.textContent = t;
	var ts_l = 100;
	if( standard.getBBox().width > l*security ) {
		ts_l = Math.round( 100 * l*security / standard.getBBox().width );
	}
	var ts_h = 100;
	if( standard.getBBox().height > h*security ) {
		ts_h = Math.round( 100 * h*security / standard.getBBox().height );
	}

	var text = document.createElementNS("http://www.w3.org/2000/svg", "text");
	text.setAttribute( "class", c  );
	text.innerHTML = '<textPath xlink:href="#' + pid + '" startOffset="50%" style="font-size:'+Math.min(ts_l,ts_h)+'%;">' + t+ '</textPath>';
	fanchart.append(text);
}
function link( pid, p, l ) {
	var ts = 100;
	if( 2 * standard_width > l ) {
		ts = Math.round( 100 * l / 2 / standard_width );
	}

	var text = document.createElementNS("http://www.w3.org/2000/svg", "text");
	text.setAttribute( "class", "link icon"  );
	text.innerHTML = '<textPath xlink:href="#' + pid + '" startOffset="50%" style="font-size:'+ts+'%;">&#x25B2;</textPath>';
	fanchart.append(text);
	text.onclick = function () {
		var oc = p.oc;
		if( oc != "" && oc != 0 ) { oc = "&oc=" + oc } else { oc = "" }
		window.location = link_to_fanchart + "p=" + p.fnk + "&n=" + p.snk + oc + "&v=" + max_gen + "&tool=" + tool;
	};
}
function no_link( pid, p, l ) {
	var ts = 100;
	if( 2 * standard_width > l ) {
		ts = Math.round( 100 * l / 2 / standard_width );
	}

	var text = document.createElementNS("http://www.w3.org/2000/svg", "text");
	text.setAttribute( "class", "no-link"  );
	text.innerHTML = '<textPath xlink:href="#' + pid + '" startOffset="50%" style="font-size:'+ts+'%;">&#x2716;</textPath>';
	fanchart.append(text);
}
function text_C3( r1, r2, a1, a2, sosa, p ) {
	var l, h;
	h = Math.abs(r2-r1)/3;
	l = path1( "tp1S"+sosa, (r2-r1)*3/4 + r1, a1, a2 );
	text2( "tp1S"+sosa, p.fn, "", l, h );
	l = path1( "tp2S"+sosa, (r2-r1)*2/4 + r1, a1, a2 );
	text2( "tp2S"+sosa, p.sn, "", l, h );
	l = path1( "tp3S"+sosa, (r2-r1)/4 + r1, a1, a2 );
	text2( "tp3S"+sosa, p.dates, "dates", l, h );
}
function text_R3( r1, r2, a1, a2, sosa, p ) {
	var my_r1, my_r2, my_a1, my_a2, my_a3, l, h;
	if( a1 >= -90 ) {
		my_r1 = r1;
		my_r2 = r2;
		my_a3 = a2 - (a2-a1)/4;
		my_a2 = a2 - (a2-a1)*2/4;
		my_a1 = a2 - (a2-a1)*3/4;
	} else {
		my_r1 = r2;
		my_r2 = r1;
		my_a3 = a1 + (a2-a1)/4;
		my_a2 = a1 + (a2-a1)*2/4;
		my_a1 = a1 + (a2-a1)*3/4;
	}
	h = Math.abs(a2-a1)/360*2*Math.PI*r1 / 3;
	l = path2( "tp1S"+sosa, my_r1, my_r2, my_a1 );
	text2( "tp1S"+sosa, p.fn, "", l, h );
	l = path2( "tp2S"+sosa, my_r1, my_r2, my_a2 );
	text2( "tp2S"+sosa, p.sn, "", l, h );
	l = path2( "tp3S"+sosa, my_r1, my_r2, my_a3 );
	text2( "tp3S"+sosa, p.dates, "dates", l, h );
}
function text_R2( r1, r2, a1, a2, sosa, p ) {
	var my_r1, my_r2, my_a1, my_a2, m, l;
	if( a1 >= -90 ) {
		my_r1 = r1;
		my_r2 = r2;
		my_a2 = a2 - (a2-a1)/3;
		my_a1 = a2 - (a2-a1)*2/3;
	} else {
		my_r1 = r2;
		my_r2 = r1;
		my_a2 = a1 + (a2-a1)/3
		my_a1 = a1 + (a2-a1)*2/3;
	}
	h = Math.abs(a2-a1)/360*2*Math.PI*r1 / 2;
	l = path2( "tp1S"+sosa, my_r1, my_r2, my_a1 );
	text2( "tp1S"+sosa, p.fn + ' ' + p.sn, "", l, h );
	l = path2( "tp2S"+sosa, my_r1, my_r2, my_a2 );
	text2( "tp2S"+sosa, p.dates, "dates", l, h );
}
function text_R1( r1, r2, a1, a2, sosa, p ) {
	var my_r1, my_r2, my_a1, my_a2, l, h;
	if( a1 >= -90 ) {
		my_r1 = r1;
		my_r2 = r2;
		my_a1 = a2 - (a2-a1)/2;
	} else {
		my_r1 = r2;
		my_r2 = r1;
		my_a1 = a1 + (a2-a1)/2;
	}
	h = Math.abs(a2-a1)/360*2*Math.PI*r1;
	l = path2( "tp1S"+sosa, my_r1, my_r2, my_a1 );
	text2( "tp1S"+sosa, p.fn + ' ' + p.sn, "", l, h );
}

function zoom (zx, zy, factor, direction) {
	var w = svg_viewbox_w;
	var h = svg_viewbox_h;
	if( direction > 0 ) {
		h = Math.round(h/factor);
		w = Math.round(w/factor);
	} else {
		h = Math.round(h*factor);
		w = Math.round(w*factor);
	}
	set_svg_viewbox(
		svg_viewbox_x + Math.round(zx * (svg_viewbox_w - w) / window_w),
		svg_viewbox_y + Math.round(zy * (svg_viewbox_h - h) / window_h),
		w, h );
}

fanchart.addEventListener( "wheel", function( event ) {
	zoom( event.clientX, event.clientY, zoom_factor, (event.deltaY < 0 ? +1 : -1) );
}, { passive: false });

var drag_state = false;
fanchart.onmousedown = function(e) {
	e.preventDefault();
	drag_state = true;
};
fanchart.onmouseup = function() {
	drag_state = false;
};
fanchart.onmousemove = function(e) {
	if( drag_state ) {
		e.preventDefault();
		set_svg_viewbox( svg_viewbox_x - Math.round(e.movementX * svg_viewbox_w / window_w),
                                 svg_viewbox_y - Math.round(e.movementY * svg_viewbox_h / window_h),
                                 svg_viewbox_w, svg_viewbox_h );
	}
};

const security = 0.95;
const zoom_factor = 1.25;
const d_all = 220;
const a_r = [   50,   50,   50,   50,   80,   70,  100,  150,  130,   90 ];
const a_m = [ "S1", "C3", "C3", "C3", "R3", "R3", "R2", "R1", "R1", "R1" ];

var ak = Object.keys(ancestor)
max_gen = 1+Math.trunc(Math.log(Number(ak[ak.length-1].replace( /^S/, "")))/Math.log(2));

var max_r = 0 ;
for( var i = 0 ; i < max_gen && i < a_r.length ; i++ ) {
	max_r += a_r[i];
}

const svg_margin = 5;
const center_x = max_r + svg_margin;
const center_y = max_r + svg_margin;

const svg_w = 2 * center_x;
const svg_h = 2 * svg_margin + max_r +
              Math.max( a_r[0], Math.round( max_r * Math.sin(Math.PI/180*(d_all-180)/2) ) );
const svg_ratio = svg_w / svg_h;

var svg_viewbox_x, svg_viewbox_y, svg_viewbox_w, svg_viewbox_h;
function set_svg_viewbox( x, y, w, h ) {
	svg_viewbox_x = x;
	svg_viewbox_y = y;
	svg_viewbox_w = w;
	svg_viewbox_h = h;
	fanchart.setAttribute( "viewBox", x + " " + y + " " + w + " " + h );
}
function fitScreen() {
	set_svg_viewbox( 0, 0, svg_w, svg_h );
}

var lieux = {};
ak.forEach( function(s) {
	var p = ancestor[s];
	if( p.birth_place !== undefined && p.birth_place != "" ) {
		if( lieux[p.birth_place] === undefined ) {
			lieux[p.birth_place] = { "cnt": 1 };
		} else {
			lieux[p.birth_place].cnt++;
		}
	}
	if( p.death_place !== undefined && p.death_place != "" ) {
		if( lieux[p.death_place] === undefined ) {
			lieux[p.death_place] = { "cnt": 1 };
		} else {
			lieux[p.death_place].cnt++;
		}
	}
	if( p.marriage_place !== undefined && p.marriage_place != "" ) {
		if( lieux[p.marriage_place] === undefined ) {
			lieux[p.marriage_place] = { "cnt": 1 };
		} else {
			lieux[p.marriage_place].cnt++;
		}
	}
	if( p.death_age !== undefined ) {
		ancestor[s].death_age = p.death_age.replace( /[^0123456789]/g, "" );
	}
});
var lieux_a = [];
for( var key in lieux ) {
	lieux_a.push([key, lieux[key]]);
}
lieux_a.sort( function(e1,e2) {
	return e2[1].cnt - e1[1].cnt
});
lieux_a.forEach( function( l, i ) {
	lieux[l[0]].c = "L"+i;
	var li = document.createElement( "li" );
	li.innerHTML = '<span id="bi-L'+i+'" class="hidden">°</span><span id="ma-L'+i+'" class="hidden">x</span><span id="de-L'+i+'" class="hidden">&dagger;</span><span class="square">■</span> ' + l[0];
	li.setAttribute( "id", "L"+i );
	li.setAttribute( "title", lieux[l[0]].cnt + " occurence(s)" );
	li.onmouseenter = function() {
		var a = document.getElementsByClassName( "L"+i );
		for( var e of a ) {
			e.classList.add( "highlight" );
		}
	};
	li.onmouseleave = function() {
		var a = document.getElementsByClassName( "L"+i );
		for( var e of a ) {
			e.classList.remove( "highlight" );
		}
	};
	places_list.append( li );
});

var standard_height, standard_width;
var standard = document.createElementNS("http://www.w3.org/2000/svg", "text");
standard.textContent = "ABCDEFGHIJKLMNOPQRSTUVW abcdefghijklmnopqrstuvwxyz";
standard.setAttribute( "id", "standard" );
standard.setAttribute( "x", center_x );
standard.setAttribute( "y", center_y );
fanchart.append(standard);
standard_width = standard.getBBox().width / standard.textContent.length;
standard_height = standard.getBBox().height;

var gen = 1;
var sosa = 1;
var r1 = 0;
var r2 = a_r[0];
var a1, a2;
var delta = d_all;

// Sosa 1
ancestor["S"+sosa].dates = ancestor["S"+sosa].dates.replace( /\s?<\/?bdo[^>]*>/g, "" );
circle_bg( "S"+sosa, r2, center_x, center_y, ancestor["S"+sosa] );
text_S1( center_x, center_y-10, ancestor["S"+sosa] );
circle( "S"+sosa, r2, center_x, center_y, ancestor["S"+sosa] );

while( true ) {
	sosa++;
	if( sosa >= (2 ** gen) ) {
		gen++;
		if( gen >= a_r.length+1 ) {
			break;
		}
		delta = delta / 2;
		r1 = r2;
		r2 = r1 + a_r[gen-1];
		a1 = -90 - d_all/2;
		a2 = a1 + delta;
	} else {
		a1 += delta;
		a2 += delta;
	}
	if( ancestor["S"+sosa] !== undefined ) {
		var same = (ancestor["S"+sosa].fn == "=" ? true : false);
		ancestor["S"+sosa].dates = ancestor["S"+sosa].dates.replace( /\s?<\/?bdo[^>]*>/g, "" );
		pie_bg( "S"+sosa, r1+10, r2, a1, a2, ancestor["S"+sosa] );
		if( ancestor["S"+sosa].fn != "?" ) {
			if( a_m[gen-1] == "C3" ) {
				text_C3( r1+10, r2, a1, a2, sosa, ancestor["S"+sosa] );
			} else if( a_m[gen-1] == "R3" && !same) {
				text_R3( r1+10, r2, a1, a2, sosa, ancestor["S"+sosa] );
			} else if( a_m[gen-1] == "R2" && !same) {
				text_R2( r1+10, r2, a1, a2, sosa, ancestor["S"+sosa] );
			} else if( a_m[gen-1] == "R1" || same) {
				text_R1( r1+10, r2, a1, a2, sosa, ancestor["S"+sosa] );
			}
		}
		if( sosa % 2 == 0 ) {
			pie_m_bg( "mS"+sosa, r1, r1+10, a1, a2+delta, ancestor["S"+sosa] );
			if( ancestor["S"+sosa].marriage_date !== undefined ) {
				var l = path1( "pmS"+sosa, r1+5, a1, a2+delta );
				text2( "pmS"+sosa, ancestor["S"+sosa].marriage_date, "", l, 10 );
			}
			pie_contour( r1, r2, a1, a2+delta );
			pie_middle( r1+10, r2, a2 );
			pie_m( "mS"+sosa, r1, r1+10, a1, a2+delta, ancestor["S"+sosa] );
		} else {
			ancestor["S"+sosa].marriage_place = ancestor["S"+(sosa-1)].marriage_place;
		}
		pie( "S"+sosa, r1+10, r2, a1, a2, ancestor["S"+sosa] );
		if( ancestor["S"+sosa].has_parents) {
			up( r1+10, a1, a2, sosa, ancestor["S"+sosa] );
		} else {
			no_up( r1+10, a1, a2, sosa, ancestor["S"+sosa] );
		}
	}
}

//document.documentElement.style.overflow = 'hidden';

const window_h = window.innerHeight;
const window_w = Math.round( window_h * svg_ratio );
const window_cx = Math.round( window_w / 2 );
const window_cy = Math.round( window_h / 2 );

fanchart.setAttribute( "height", window_h );
fanchart.setAttribute( "width", window_w );

// Tools
document.getElementById("b-home").onclick = function() {
	window.location = link_to_person;
};
document.getElementById("b-refresh").onclick = function() {
	fitScreen();
};
document.getElementById("b-zoom-in").onclick = function() {
	zoom( window_cx, window_cy, zoom_factor, +1 );
};
document.getElementById("b-zoom-out").onclick = function() {
	zoom( window_cx, window_cy, zoom_factor, -1 );
};
document.getElementById("b-gen-add").onclick = function() {
	if( max_gen < 10 ) {
		var p = ancestor["S1"];
		var oc = p.oc;
		if( oc != "" && oc != 0 ) { oc = "&oc=" + oc } else { oc = "" }
		window.location = link_to_person + "m=A&t=FC&mono=" + mono + "&tool=" + tool + "&p=" + p.fnk + "&n=" + p.snk + oc + "&v=" + (max_gen+1);
	}
};
document.getElementById("b-gen-del").onclick = function() {
	if( max_gen > 1 ) {
		var p = ancestor["S1"];
		var oc = p.oc;
		if( oc != "" && oc != 0 ) { oc = "&oc=" + oc } else { oc = "" }
		window.location = link_to_person + "m=A&t=FC&mono=" + mono + "&tool=" + tool + "&p=" + p.fnk + "&n=" + p.snk + oc + "&v=" + (max_gen-1);
	}
};
document.getElementById("b-places-hl").onclick = function() {
	document.body.className = "places-list place_hl";
	tool = "place_hl";
};
document.getElementById("b-places-colorise").onclick = function() {
	document.body.className = "places-list place_color";
	tool = "place_color";
};
document.getElementById("b-death-age").onclick = function() {
	document.body.className = "death-age";
	tool = "death-age";
};
document.getElementById("b-no-tool").onclick = function() {
	document.body.className = "";
	tool = "";
};

// Initial state for tools
if( tool == "place_hl" ) {
	document.body.className = "places-list place_hl";
} else if( tool == "place_color" ) {
	document.body.className = "places-list place_color";
} else if( tool == "death-age" ) {
	document.body.className = "death-age";
}

fitScreen();
