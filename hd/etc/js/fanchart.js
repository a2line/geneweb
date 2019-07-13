var fanchart = document.getElementById( "fanchart" );

const center_x = 700;
const center_y = 710;
function pos_x( r, a ) {
	return center_x + r * Math.cos( Math.PI / 180 * a );
}
function pos_y( r, a ) {
	return center_y + r * Math.sin( Math.PI / 180 * a );
}
function up( r, a1, a2, sosa, p ) {
	path1( "tpiS"+sosa, r, a1, a2 );
	link( "tpiS"+sosa, p );
}
function pie( id, r1, r2, a1, a2, p ) {
	var a = document.createElementNS( "http://www.w3.org/2000/svg", "a" );
	a.setAttributeNS( "http://www.w3.org/1999/xlink", "href", link_to_person + "p=" + p.fn + "&n=" + p.sn + "&oc=" + p.oc );
	fanchart.append(a);

	var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
	path.setAttribute( "d",
		 'M ' + pos_x(r2, a1) + ',' + pos_y(r2, a1) +
		' A ' + r2 + ' ' + r2 + ' 0 0 1 ' + pos_x(r2, a2) + ',' + pos_y(r2, a2) +
		' L ' + pos_x(r1, a2) + ',' + pos_y(r1, a2) +
		' A ' + r1 + ' ' + r1 + ' 0 0 0 ' + pos_x(r1, a1) + ',' + pos_y(r1, a1) +
		' Z'
	);
	path.setAttribute( "id", id );
	a.append(path);

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
function circle( id, r, cx, cy, p ) {
	var a = document.createElementNS( "http://www.w3.org/2000/svg", "a" );
	a.setAttributeNS( "http://www.w3.org/1999/xlink", "href", link_to_person + "p=" + p.fn + "&n=" + p.sn + "&oc=" + p.oc );
	fanchart.append(a);

	var circle = document.createElementNS("http://www.w3.org/2000/svg", "circle");
	circle.setAttribute( "cx", cx );
	circle.setAttribute( "cy", cy );
	circle.setAttribute( "r", r );
	circle.setAttribute( "id", id );
	a.append(circle);
}
function text_S1( x, y, p ) {
	var text = document.createElementNS("http://www.w3.org/2000/svg", "text");
	text.setAttribute( "x", x );
	text.setAttribute( "y", y );
	text.setAttribute( "class", "gen1" );
	var ts1 = 100;
	if( (2+p.fn.length) * standard_width > 100 ) {
		ts1 = Math.round( 100 * 100 / (2+p.fn.length) / standard_width );
	}
	var ts2 = 100;
	if( (2+p.sn.length) * standard_width > 100 ) {
		ts2 = Math.round( 100 * 100 / (2+p.sn.length) / standard_width );
	}
	text.innerHTML = '<tspan style="font-size:'+ts1+'%">' + p.fn + '</tspan><tspan x="' + x + '" dy="15" style="font-size:'+ts2+'%">' + p.sn + '</tspan><tspan class="dates" x="' + x + '" dy="15">' + p.dates + '</tspan>';
	fanchart.append(text);
}

function path1( id, r, a1, a2 ) {
	var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
	path.setAttribute( "class", "none" );
	path.setAttribute( "d",
		 'M ' + pos_x(r, a1) + ',' + pos_y(r, a1) +
		' A ' + r + ' ' + r + ' 0 0 1 ' + pos_x(r, a2) + ',' + pos_y(r, a2)
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
	var ts_l = 100;
	if( (2+t.length) * standard_width > l ) {
		ts_l = Math.round( 100 * l / (2+t.length) / standard_width );
	}
	var ts_h = 100;
	if( standard_height > h ) {
		ts_h = Math.round( 100 * h / standard_height );
	}

	var text = document.createElementNS("http://www.w3.org/2000/svg", "text");
	text.setAttribute( "class", c  );
	text.innerHTML = '<textPath xlink:href="#' + pid + '" startOffset="50%" style="font-size:'+Math.min(ts_l,ts_h)+'%;">' + t+ '</textPath>';
	fanchart.append(text);
}
function link( pid, p ) {
	var a = document.createElementNS( "http://www.w3.org/2000/svg", "a" );
	a.setAttributeNS( "http://www.w3.org/1999/xlink", "href", link_to_fanchart + "p=" + p.fn + "&n=" + p.sn + "&oc=" + p.oc );
	fanchart.append(a);

	var text = document.createElementNS("http://www.w3.org/2000/svg", "text");
	text.setAttribute( "class", "icon"  );
	text.innerHTML = '<textPath xlink:href="#' + pid + '" startOffset="50%">&#x25B2;</textPath>';
	a.append(text);
}
function text_M1( r1, r2, a1, a2, sosa, p, gen ) {
	var l, h;
	h = Math.abs(r2-r1)/3;
	l = path1( "tp1S"+sosa, (r2-r1)*2/3 + r1, a1, a2 );
	text2( "tp1S"+sosa, p.fn, gen, l, h );
	l = path1( "tp2S"+sosa, (r2-r1)/3 + r1, a1, a2 );
	text2( "tp2S"+sosa, p.sn, gen, l, h );
	l = path1( "tp3S"+sosa, r1+2, a1, a2 );
	text2( "tp3S"+sosa, p.dates, "dates", l, h );
}
function text_M2( r1, r2, a1, a2, sosa, p, gen ) {
	var my_r1, my_r2, my_a1, my_a2, my_a3, l, h;
	if( a1 >= -90 ) {
		my_r1 = r1;
		my_r2 = r2;
		my_a3 = a2 - (a2-a1)/12;
		my_a2 = a2 - (a2-a1)*5/12;
		my_a1 = a2 - (a2-a1)*9/12;
	} else {
		my_r1 = r2;
		my_r2 = r1;
		my_a3 = a1 + (a2-a1)/12;
		my_a2 = a1 + (a2-a1)*5/12;
		my_a1 = a1 + (a2-a1)*9/12;
	}
	h = Math.abs(a2-a1)/360*2*Math.PI*r1 / 3;
	l = path2( "tp1S"+sosa, my_r1, my_r2, my_a1 );
	text2( "tp1S"+sosa, p.fn, gen, l, h );
	l = path2( "tp2S"+sosa, my_r1, my_r2, my_a2 );
	text2( "tp2S"+sosa, p.sn, gen, l, h );
	l = path2( "tp3S"+sosa, my_r1, my_r2, my_a3 );
	text2( "tp3S"+sosa, p.dates, "dates", l, h );
}
function text_M3( r1, r2, a1, a2, sosa, p, gen ) {
	var my_r1, my_r2, my_a1, my_a2, m, l;
	if( a1 >= -90 ) {
		my_r1 = r1;
		my_r2 = r2;
		my_a2 = a2 - 0.5;
		my_a1 = a2 - (a2-a1)*6/12;
	} else {
		my_r1 = r2;
		my_r2 = r1;
		my_a2 = a1 + 0.5;
		my_a1 = a1 + (a2-a1)*6/12;
	}
	h = Math.abs(a2-a1)/360*2*Math.PI*r1 / 2;
	l = path2( "tp1S"+sosa, my_r1, my_r2, my_a1 );
	text2( "tp1S"+sosa, p.fn + ' ' + p.sn, gen, l, h );
	l = path2( "tp2S"+sosa, my_r1, my_r2, my_a2 );
	text2( "tp2S"+sosa, p.dates, "dates", l, h );
}
function text_M4( r1, r2, a1, a2, sosa, p, gen ) {
	var my_r1, my_r2, my_a1, my_a2, l; h;
	if( a1 >= -90 ) {
		my_r1 = r1;
		my_r2 = r2;
		my_a1 = a2 - (a2-a1)/4;
	} else {
		my_r1 = r2;
		my_r2 = r1;
		my_a1 = a1 + (a2-a1)/4;
	}
	h = Math.abs(a2-a1)/360*2*Math.PI*r1;
	l = path2( "tp1S"+sosa, my_r1, my_r2, my_a1 );
	text2( "tp1S"+sosa, p.fn + ' ' + p.sn, gen, l, h );
}

var standard_height, standard_width;
function text_standard() {
	var standard = document.createElementNS("http://www.w3.org/2000/svg", "text");
	standard.textContent = "ABCDEFGHIJKLMNOPQRSTUVW abcdefghijklmnopqrstuvwxyz";
	standard.setAttribute( "id", "standard" );
	standard.setAttribute( "x", center_x );
	standard.setAttribute( "y", center_y );
	fanchart.append(standard);

	standard_width = standard.getBBox().width / standard.textContent.length;
	standard_height = standard.getBBox().height;
};

var gen = 1;
var sosa = 1;
var delta = 220;
var r1 = 0;
var r2 = 50;
var a1, a2;

var t_size = text_standard();

// Sosa 1
text_S1( center_x, center_y-10, ancestor["S"+sosa] );
circle( "S"+sosa, r2, center_x, center_y, ancestor["S"+sosa] );

while( true ) {
	sosa++;
	if( sosa >= (2 ** gen) ) {
		gen++;
		if( gen >= 9 ) {
			break;
		}
		delta = delta / 2;
		r1 = r2;
		if( delta > 14 ) {
			r2 = r1 + 50;
		} else if ( delta > 4 ) {
			r2 = r1 + 100;
		} else {
			r2 = r1 + 150;
		}
		a1 = -200;
		a2 = a1 + delta;
	} else {
		a1 += delta;
		a2 += delta;
	}
	if( ancestor["S"+sosa] !== undefined ) {
		if( delta > 14 ) {
			text_M1( r1, r2, a1, a2, sosa, ancestor["S"+sosa], "gen"+gen );
		} else if( delta > 4 ) {
			text_M2( r1, r2, a1, a2, sosa, ancestor["S"+sosa], "gen"+gen );
		} else if( delta > 2 ){
			text_M3( r1, r2, a1, a2, sosa, ancestor["S"+sosa], "gen"+gen );
		} else {
			text_M4( r1, r2, a1, a2, sosa, ancestor["S"+sosa], "gen"+gen );
		}
		pie( "S"+sosa, r1, r2, a1, a2, ancestor["S"+sosa] );
		up( r1+4, a1, a2, sosa, ancestor["S"+sosa] );
	}
}
