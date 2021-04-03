$(function() {
    'use strict';

    // begin ggm-twbs: this code beats the exported .html file into
    // twbs-shape

    $('#content').addClass("container");

    // here we are assuming the first child is the table of contents.
    // XXX if there is no toc?  XXX if it elsewhere in the .html file?
    $("#table-of-contents")
        .children()
        .first()
        .remove();
    $("#table-of-contents")
        .children()
        .wrapAll('<div class="col-md-3 col-md-push-9"></div>');
    $("#table-of-contents")
        .children()
        .unwrap();
    $("#table-of-contents").remove();

    // fixed olsun
    $('#text-table-of-contents').addClass("bs-docs-sidebar");

    // for scrollspy (below), add class nav to the toc
    $("#text-table-of-contents ul").addClass("nav");

    // then, *each* (not *all*, though maybe that is what we should
    // do?) of the rest are wrapped in the col-md-9/col-md-pull-3
    // div's
    $("#content").children()
        .first()
        .siblings()
        .wrapAll('<div class="col-md-9 col-md-pull-3"></div>');

    $('#content').children().wrapAll('<div class="row">');
    // end ggm-twbs


    $('.bs-docs-sidebar li').first().addClass('active');

    $(document.body).scrollspy({target: '.bs-docs-sidebar'});

    $('.bs-docs-sidebar').affix();
});
