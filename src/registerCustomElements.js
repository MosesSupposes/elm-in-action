class RangeSlider extends HTMLElement {
  connectedCallback() {
    const input = document.createElement("input")
    this.appendChild(input)
  }
}

window.customElements.define("range-slider", RangeSlider)
