window.addEventListener("scroll", function() {
    if (window.scrollY > 400) {
        $('.navbar').fadeOut();
    } else {
        $('.navbar').fadeIn();
    }
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
