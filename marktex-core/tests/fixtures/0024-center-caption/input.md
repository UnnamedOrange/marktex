![](assets/1.jpg)
<center>图片的标题</center>

<center>不算图片的标题</center>

![first-image](assets/2.jpg)

<center>图片的标题</center>
<center>不算图片的标题</center>

一个内联图片 ![内联图片示例](assets/3.jpg)。
<center>不算图片的标题</center>

<img src="assets/4.jpg" />

<center>图 1: 带有 label 的图片的标题</center>

<center>something</center>

<img src="assets/5.jpg" alt="HTML 图片示例" />
<center>图 some:不要空格也可以</center>

<img src="assets/6.jpg" alt="HTML 图片示例" style="zoom: 33%;" />


<center>图 another:    有多个前置空格也会被省略  </center>

<img src="assets/7.jpg" style="width: 33%;" />
<center>图 yet-another</center>
<img src="assets/8.jpg" style="width:50%;" />
<center>图 yet-yet-another: 也就是说也可以不要标题</center>

<img src="a **!"/>
something
<center>Nope</center>

![](test_image.jpg)
<center>只要不是图开头:再拽的冒号也没用</center>

<img src="a **!"/>


<center>图:冒号中间没东西，也认为标签是空的</center>

![](test_image.jpg)

<center>图 标签 空格 :  冒号前面有额外空格，原样算上应该最简单</center>

![](test_image.jpg)



<center>图1: 忘记打空格了，不允许！</center>


![](test_image.jpg)
<center>图  : 打了空格是空的，算识别出标签，但是标签为空，就不写了。</center>

![](test_image.jpg)
<center>图   标签: 前面打了好几个空格，忽视它。</center>
![](test_image.jpg)
<center>图   标签: </center>
