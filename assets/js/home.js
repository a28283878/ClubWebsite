window.addEventListener("scroll", function() {
    if (window.scrollY > 400) {
        $('.navbar').fadeOut();
    } else {
        $('.navbar').fadeIn();
    }
}, false);

window.addEventListener("scroll", function() {
    if(window.scrollY > 350) $('.section1').fadeTo(400, 1);
}, false);

$('.lesson').click(function(event) {
    var lessonId = "#" + $(this).attr('value');
    $(lessonId).fadeIn();
    $('#dark').fadeIn();
})

$('#dark').click(function() {
    $('.lessonDetail').fadeOut();
    $('#dark').fadeOut();
})
