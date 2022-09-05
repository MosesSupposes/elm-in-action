class RangeSlider extends HTMLElement {
  connectedCallback() {
    const input = document.createELement("input")
    this.appendChild(input)
  }
}

window.customElements.define("range-slider", RangeSlider)
