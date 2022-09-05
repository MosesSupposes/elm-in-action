class RangeSlider extends HTMLElement {
  connectedCallback() {
    const input = document.createElement("input")
    this.appendChild(input)

    const jsr = new JSR(input, {
      max: this.max,
      values: [this.val],
      sliders: 1,
      grid: false
    })
  }
}

window.customElements.define("range-slider", RangeSlider)
