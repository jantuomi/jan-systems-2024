window.Unfold = {
    activate: async function (opts) {
        const separator = opts.separator || "hr";
        const content = opts.content || "main";
        const theme = opts.theme || "moon";
        const revealCdn = opts.revealCdn || "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.6.0";
        const revealPlugins = opts.revealPlugins || []; // e.g. ["notes"]
        const revealOptions = opts.revealOptions || {};
        const revealCss = opts.revealCss || "";

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

        const titleSlide = document.createElement("section");
        const titleH1 = document.createElement("h1");
        titleH1.textContent = document.title;
        titleSlide.appendChild(titleH1);
        slides.appendChild(titleSlide);

        let currentSlide = document.createElement("section");
        for (const node of nodes) {
            if (node.hasAttribute && node.hasAttribute("data-unfold-hidden")) {
                continue;
            }

            if (node.tagName?.toLowerCase() === separator.toLowerCase()) {
                slides.appendChild(currentSlide);
                currentSlide = document.createElement("section");
            } else if (node.matches && node.matches("figure:has(img)")) {
                const figcaption = node.querySelector("figcaption");
                if (figcaption) {
                    const small = document.createElement("small");
                    small.textContent = figcaption.textContent;
                    small.style.display = "block";
                    currentSlide.appendChild(small);
                }

                const img = node.querySelector("img");
                img.classList.add("r-stretch");
                currentSlide.appendChild(img);
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

        for (const plugin of revealPlugins) {
            const pluginJsTag = document.createElement("script");
            pluginJsTag.src = `${revealCdn}/plugin/${plugin}/${plugin}.js`;
            newHead.appendChild(pluginJsTag);
        }

        const revealJsTag = document.createElement("script");
        revealJsTag.src = `${revealCdn}/js/reveal.min.js`;
        revealJsTag.onload = () => {
            // evaluate plugins in a dynamic scope in order to access plugins etc.
            const evaledPlugins = revealPlugins.map(p => {
                const symbolName = `Reveal${p.charAt(0).toUpperCase() + p.slice(1)}`;
                return eval(symbolName);
            });
            // activate Reveal slideshow
            const reveal = { ...revealOptions, plugins: evaledPlugins };
            console.log(reveal);
            Reveal.initialize(reveal);
            // fix vertical positioning issue that happens often
            setTimeout(() => Reveal.sync(), 100);
        };
        newHead.appendChild(revealJsTag);

        const revealCustomCssTag = document.createElement("style");
        revealCustomCssTag.innerHTML = revealCss;
        newHead.appendChild(revealCustomCssTag);

        document.head.replaceWith(newHead);
        document.body.replaceWith(newBody);
    },
    enable: function (opts = {}) {
        // if the URL has ?unfold=1, activate the slideshow
        const qp = new URLSearchParams(location.search);
        if (qp.get("unfold") === "1") {
            void Unfold.activate(opts)
        }

        // listen for clicks on links with ?unfold=1
        // to activate the slideshow without a page reload for a smoother experience
        const unfoldLinks = document.querySelectorAll("a[href*='unfold=1']");
        for (const link of unfoldLinks) {
            link.addEventListener("click", function (e) {
                e.preventDefault();
                history.pushState({}, "", this.href);
                void Unfold.activate(opts);
            });
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

