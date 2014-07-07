$(function() {
    var doc = $(document);

    var added;
    doc.scroll(function(e){
        if (doc.scrollTop() > 5) {
            if (added) return;
            added = true;
            $('body').addClass('scroll');
        } else {
            $('body').removeClass('scroll');
            added = false;
        }
    })
});

$(function(){
    var prev;
    var n = 0;

    var menu = $('#menu ul');
    var headings = $('.command-title').map(function(i, el){
        var name = el.id.replace(/cmd-/, '');
        menu.append($('<li><a href="#' + el.id + '">' + name + '</a></li>'));

        return {
            top: $(el).offset().top,
            id: el.id
        }
    });

    function closest() {
        var h;
        var top = $(window).scrollTop();
        var i = headings.length;
        while (i--) {
            h = headings[i];
            if (top >= h.top) return h;
        }
    }

    $(document).scroll(function(){
        var h = closest();
        if (!h) return;

        if (prev) {
            prev.removeClass('active');
            prev.parent().parent().removeClass('active');
        }

        var a = $('a[href="#' + h.id + '"]');
        a.addClass('active');
        a.parent().parent().addClass('active');

        prev = a;
    });
});
