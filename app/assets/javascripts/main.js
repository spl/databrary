// clean dbjs namespace
var dbjs = {};

// setup site features
(function () {
    var isTouch = 'ontouchstart' in document.documentElement;

    dbjs.toggleFold = function (fold, folder, folded) {
        var $folders = $(fold + ' ' + folder),
            $foldeds = $(fold + ' ' + folded);

        $folders.click(function (e) {
            $(this).next(folded).slideToggle();
            $(this).toggleClass('unfolded');
        });

        $folders.prepend('<span class="arrow"></span>')
        $foldeds.toggle();
    }

    dbjs.menuReduce = function (menu, position) {
        var $menu = $(menu),
            $position = $(position),
            $origin = $menu.parent(),
            $menuHeader = $menu.find('h1'),
            $menuLink = $('<button class="hide"></button>'),
            toggle = false;

        function menuReducer() {
            if (toggle == false && $menu.css('z-index') == 1) {
                $menu.addClass('reduced').prependTo($position);
                $menuHeader.remove();
                $menuLink.removeClass('hide');
                toggle = true;
            } else if (toggle == true && $menu.css('z-index') == 'auto') {
                $menu.removeClass('reduced').prependTo($origin);
                $menuHeader.prependTo($menu);
                $menuLink.addClass('hide');
                toggle = false;
            }
        }

        $(window).resize(function (e) {
            menuReducer();
        });

        menuReducer();

        $menuLink.prependTo($menu);
        $menuLink.click(function (e) {
            $menu.toggleClass('dropped');
        });
    }
}());

// event bindings
// ...

// initialization
$(document).ready(function () {
    // registration should only appear on the pages it's need. I've added it to-do.
    dbjs.toggleFold('.question', 'h2', 'div');

    dbjs.menuReduce('#static_links', '#body_main > article > h1:first-child');


    // refactor AND switch to modals!!!
    $('.drop_down .drop_toggle:not(".nil") a').click(function (e) {
        e.preventDefault();
    });
    $('.drop_down .drop_toggle').click(function (e) {
        var dropDown = $(e.currentTarget).parent();

        dropDown.toggleClass('dropped');

        e.preventDefault();
    });

    // animation helpers REFACTOR
    var waitForFinalEvent = (function () {
        var timers = {};

        return function (callback, ms, uniqueId) {
            if (!uniqueId) {
                uniqueId = "Don't call this twice without a uniqueId";
            }

            if (timers[uniqueId]) {
                clearTimeout(timers[uniqueId]);
            }

            timers[uniqueId] = setTimeout(callback, ms);
        };
    })();
});
