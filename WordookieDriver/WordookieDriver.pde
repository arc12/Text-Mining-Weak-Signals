/*
    Modified Adam Cooper 2010 from an original:
    Copyright 2009 Michael Ogawa

    This file is part of Wordookie.

    Wordookie is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Wordookie is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Wordookie.  If not, see <http://www.gnu.org/licenses/>.
*/

import wordookie.*;
import wordookie.util.*;
import wordookie.parsers.*;
import java.io.*;

WordWeightParser parser;
Layout layout;
java.util.List words;
Iterator itr;
PFont font;
int startTime;
int[] colourScheme;
int scaleMin;
int scaleMax;

final String FILENAME = "AB Pre-sig Wordle subQ3";//append .txt to get input name, .jpg for stored image
 
void setup()
{
  size( 1024, 768 );
  smooth();
  // ======= Style Settings =======
  /*
  Layout options:
    ANYWAY - Words are angled "any which way."
    HALF_AND_HALF - Half the words are angled horizontally and the other half vertically.
    HORIZONTAL - All words are angled horizontally.
    MOSTLY_HORIZONTAL - Most words are angled horizontally; the rest are vertical.
    MOSTLY_VERTICAL - Most of the words are angled vertically; the rest are horizontal.
    VERTICAL - All words are angled vertically.
  Colour scheme options:
    Autumn - Reds and oranges.
    BlueIce - A white-to-blue theme.
    EasterEgg - Pastel colors.
  */  
  font = createFont( "SansSerif", 32 );//the size is effectively irrelevant - see scale values
  scaleMin=16;//default 16
  scaleMax=100;//default 100. Reduce if the weight distribution is quite uniform or top-heavy (layout cannot fit words in!)
  int bgColour = color(0);
  background(bgColour);
  layout = new Layout( this, bgColour );
  layout.setAngleType( layout.HALF_AND_HALF );
  colourScheme = ColorStuff.Autumn;
  // == end Style Settings ==
  
  parser = new WordWeightParser();
  
  InputStream in = createInput( FILENAME+".txt" );
  try
  {
    parser.load( in );
  }
  catch( Exception ex )
  {
    ex.printStackTrace();
  }
  
  words = parser.getWords();
  Collections.sort( words );
  itr = words.iterator();
  
  startTime = millis();
}

void draw()
{
  if( itr.hasNext() )
  {
    Word word = (Word)itr.next();
    println( word.toString() );
    int fontSize = (int)map( word.weight, parser.getMinWeight(), parser.getMaxWeight(), 16, 50 );
    word.font = font;
    word.fontSize = fontSize;
    layout.doLayout( word );
    fill(colourScheme[ (int)random(colourScheme.length) ] );
    layout.paintWord( word );
  }
  else
  {
    save(FILENAME+".jpg");
    int endTime = millis();
    println( "Done: " + (endTime - startTime) + " msec" );
    noLoop();
  }
}


