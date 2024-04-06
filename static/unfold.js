window.Unfold = {
    activate: function (opts) {
        const separator = opts.separator || "hr";
        const content = opts.content || "main";
        const theme = opts.theme || "moon";
        const revealCdn = opts.revealCdn || "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.6.0";

        const contentElem = document.querySelector(content);
        if (!contentElem) {
            console.error(`Unfold: content element not found`);
            return;
        }

        const nodes = [...contentElem.childNodes];

        const newBody = document.createElement("body");
        newBody.setAttribute("class", "reveal");
        const slides = document.createElement("div");
        slides.setAttribute("class", "slides");
        newBody.appendChild(slides);

        const newHead = document.createElement("head");

        let currentSlide = document.createElement("section");
        for (const node of nodes) {
            if (node.hasAttribute && node.hasAttribute("data-unfold-hidden")) {
                continue;
            }

            if (node.tagName?.toLowerCase() === separator.toLowerCase()) {
                slides.appendChild(currentSlide);
                currentSlide = document.createElement("section");
            } else {
                currentSlide.appendChild(node);
            }
        }
        slides.appendChild(currentSlide);

        const revealCssTag = document.createElement("link");
        revealCssTag.rel = "stylesheet";
        revealCssTag.href = `${revealCdn}/css/reveal.min.css`;
        newHead.appendChild(revealCssTag);

        const revealThemeCssTag = document.createElement("link");
        revealThemeCssTag.rel = "stylesheet";
        revealThemeCssTag.href = `${revealCdn}/css/theme/${theme}.css`;
        newHead.appendChild(revealThemeCssTag);

        const revealJsTag = document.createElement("script");
        revealJsTag.src = `${revealCdn}/js/reveal.min.js`;
        revealJsTag.onload = () => {
	    // activate Reveal slideshow
            Reveal.initialize();
	    // fix vertical positioning issue that happens often
	    setTimeout(() => Reveal.sync(), 100);
        };
        newHead.appendChild(revealJsTag);

        document.head.replaceWith(newHead);
        document.body.replaceWith(newBody);
    },
    bind: function (selector, opts = {}) {
        const elem = document.querySelector(selector);
        elem.addEventListener("click", () => {
            Unfold.activate(opts)
            // enable back button by hooking into History API
            const qp = new URLSearchParams(location.search);
            qp.set("unfold", "1");
            history.pushState({}, "", `${location.protocol}//${location.host}${location.pathname}?${qp.toString()}`);
        })

        const qp = new URLSearchParams(location.search);
        if (qp.get("unfold") === "1") {
            Unfold.activate(opts)
        }
    }
}

const previousOnPopState = window.onpopstate;
window.onpopstate = function() {
    if (typeof previousOnPopState === "function") {
	    previousOnPopState();
    }
    location.reload();
};

