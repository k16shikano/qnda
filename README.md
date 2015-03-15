# qnda
qnda makes an epub file from several html files according to a main file which imports each of them.

## Feature
qnda is written with HXT, Haskell Xml Tool. It means you don't need to write each html file in proper XHTML/HTML5. Instead, you can make use of a html of your fevorite flavor, and then define the conversion rule to XHTML/HTML5 using Arrow. 

qnda generates usual sequential numbers within a book. You can write your document without hard-corded sequential numbers like "Chapter 1" or "Fig-1.2". In addition to that, these numbers are automatically resolved when you want to refer them from somewhere else in your book. So, you can write

    <p>For more details, see <a id="foo"/>.</p>
    <figure>
      <img src="foo.png"/>
      <caption id="foo">details of foo</caption>
    </figure>

instead of,

    <p>For more details, see Fig-1.3.</p>
    <figure>
      <img src="foo.png"/>
      <caption id="foo">Fig-1.3: details of foo</caption>
    </figure>

The only requirement is the main file. It will be like below. 

    <book>
    <bookinfo>
      <authors>
        <name role="author">John Doe</name>
      </authors>
      <booktitle>Awesome Book</booktitle>
      <printings>
        <printing date="Jan. 2000">1st edition.</printing>
        <printing date="Feb. 2015">2nd edition.</printing>
      </printings>
      <copyright year="2015">Keiichiro Shikano</copyright>
      <isbn>978-xxxxxxxxxx</isbn>
    </bookinfo>

    <frontmatter/>
      <a href="preface.html"><include>preface</include></a>
    <mainmatter/>
      <a href="ch01.html"><include>ch01</include></a>
      <a href="ch02.html"><include>ch02</include></a>
      <a href="ch03.html"><include>ch03</include></a>
    <appendix/>
      <a href="app.html"><include>app</include></a>
    </book>
 
 
## Disadvantage

You might as well write Arrow.


## License

Copyright (c) 2015 SHIKANO keiichiro k16.shikano@gmail.com

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
