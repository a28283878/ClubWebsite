// window.addEventListener("scroll", function() {
//     if (window.scrollY > 400) {
//         $('.navbar').fadeOut();
//     } else {
//         $('.navbar').fadeIn();
//     }
// }, false);

window.addEventListener("scroll", function() {
    if(window.scrollY > 350) $('.section1').fadeTo(400, 1);
}, false);

$('.lesson').click(function(event) {
    var lessonId = "#" + $(this).attr('value');
    $('.modal-close').addClass('visible');
    $('#cancel').fadeIn();
    $(lessonId).fadeIn();
    $('#dark').fadeIn();
})

$('#dark').click(function() {
	$('.modal-close').removeClass('visible');
    $('.lessonDetail').fadeOut();
    $('#dark').fadeOut();
})

$('.modal-close').click(function() {
	$('.modal-close').removeClass('visible');
    $('.lessonDetail').fadeOut();
    $('#dark').fadeOut();
})


jQuery(document).ready(function($){
	var timelineBlocks = $('.item'),
		offset = 0.8;

	//hide timeline blocks which are outside the viewport
	hideBlocks(timelineBlocks, offset);

	//on scolling, show/animate timeline blocks when enter the viewport
	$(window).on('scroll', function(){
		(!window.requestAnimationFrame) 
			? setTimeout(function(){ showBlocks(timelineBlocks, offset); }, 100)
			: window.requestAnimationFrame(function(){ showBlocks(timelineBlocks, offset); });
	});

	function hideBlocks(blocks, offset) {
		blocks.each(function(){
			( $(this).offset().top > $(window).scrollTop()+$(window).height()*offset ) && $(this).addClass('is-hidden');
		});
	}

	function showBlocks(blocks, offset) {
		blocks.each(function(){
			( $(this).offset().top <= $(window).scrollTop()+$(window).height()*offset && $(this).hasClass('is-hidden') ) && $(this).removeClass('is-hidden').addClass('bounce-in');
		});
	}
});