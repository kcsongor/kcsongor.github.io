async function initialize(root) {
  const status = root.querySelector("[data-status]");
  try {
    const threeUrl =
      "https://cdn.jsdelivr.net/npm/three@0.180.0/build/three.module.js";
    const THREE = await import(/* @vite-ignore */ threeUrl);
    const clamp = (n, a, b) => Math.max(a, Math.min(b, n));
    const BLOCK_START = 37,
      LOOP_END = 44.9999,
      LANDING_SECONDS = 0.1;
    const sub = ["₀", "₁"],
      a = [
        [1, 2],
        [3, 4],
      ],
      b = [
        [2, 1],
        [1, 2],
      ];
    const reduced = matchMedia("(prefers-reduced-motion: reduce)").matches;
    const timeline = root.querySelector("input"),
      play = root.querySelector("[data-play]");
    const probe = root.querySelector(".bt-colour-probe");
    const colourCanvas = document.createElement("canvas");
    colourCanvas.width = colourCanvas.height = 1;
    const colourContext = colourCanvas.getContext("2d", {
      willReadFrequently: true,
    });
    const palette = {};
    function readColour(variable) {
      probe.style.color = `var(${variable})`;
      colourContext.clearRect(0, 0, 1, 1);
      colourContext.fillStyle = getComputedStyle(probe).color;
      colourContext.fillRect(0, 0, 1, 1);
      const rgb = colourContext.getImageData(0, 0, 1, 1).data;
      return new THREE.Color().setRGB(
        rgb[0] / 255,
        rgb[1] / 255,
        rgb[2] / 255,
        THREE.SRGBColorSpace
      );
    }
    function theme() {
      palette.bg = readColour("--background");
      palette.fg = readColour("--foreground");
      palette.first = readColour("--bt-first");
      palette.second = readColour("--bt-second");
      palette.sum = palette.first.clone().lerp(palette.second, 0.5);
      palette.idle = readColour("--matrix-idle");
      const backgroundLight =
        palette.bg.r + palette.bg.g + palette.bg.b >
        palette.fg.r + palette.fg.g + palette.fg.b;
      palette.darkText = backgroundLight
        ? "var(--foreground)"
        : "var(--background)";
      palette.lightText = backgroundLight
        ? "var(--background)"
        : "var(--foreground)";
    }
    theme();
    const box = new THREE.BoxGeometry(1, 1, 1),
      sphere = new THREE.SphereGeometry(1, 10, 8);
    const temp = new THREE.Vector3();
    const panels = {};
    function label(panel, text, className = "") {
      const el = document.createElement("span");
      el.className = "bt-label " + className;
      el.textContent = text;
      panel.host.append(el);
      return el;
    }
    function projectLabel(panel, el, point) {
      temp.copy(point).project(panel.camera);
      el.style.left = ((temp.x + 1) * panel.width) / 2 + "px";
      el.style.top = ((1 - temp.y) * panel.height) / 2 + "px";
    }
    function rectangle(width, depth) {
      const pts = [
        [-width / 2, -depth / 2],
        [width / 2, -depth / 2],
        [width / 2, depth / 2],
        [-width / 2, depth / 2],
        [-width / 2, -depth / 2],
      ].map(([x, z]) => new THREE.Vector3(x, 0, z));
      return new THREE.Line(
        new THREE.BufferGeometry().setFromPoints(pts),
        new THREE.LineBasicMaterial({
          color: palette.fg,
          transparent: true,
          opacity: 0.8,
        })
      );
    }
    function makePanel(size) {
      const host = root.querySelector(`[data-scene="${size}"]`);
      const renderer = new THREE.WebGLRenderer({
        alpha: true,
        antialias: true,
        powerPreference: "low-power",
      });
      renderer.setPixelRatio(Math.min(devicePixelRatio, 2));
      renderer.setClearAlpha(0);
      renderer.outputColorSpace = THREE.SRGBColorSpace;
      host.prepend(renderer.domElement);
      const scene = new THREE.Scene(),
        camera = new THREE.OrthographicCamera(-9, 9, 3, -3, 0.1, 100);
      camera.position.set(0, 12, 0);
      camera.up.set(0, 0, -1);
      camera.lookAt(0, 0, 0);
      camera.updateMatrixWorld();
      const panel = {
        host,
        renderer,
        scene,
        camera,
        width: 1,
        height: 1,
        n: size === "small" ? 2 : 8,
        cells: {},
        frames: {},
        retained: {},
        focus: {},
        outputFocus: [],
        blockLabels: {},
        names: [],
        routes: [],
        events: [],
        contributions: [],
        gap: 0,
      };
      const n = panel.n,
        pitch = n === 2 ? 1.5 : 0.44,
        side = n === 2 ? 1.34 : 0.385;
      for (const m of ["A", "B", "C"]) {
        panel.cells[m] = Array.from({ length: n }, () => []);
        panel.frames[m] = [];
        const cursor = rectangle(1, 1);
        cursor.position.y = 0.8;
        scene.add(cursor);
        panel.focus[m] = { cursor, target: null, initialized: false };
        panel.blockLabels[m] = [];
        if (n === 8 && m !== "C") {
          const tag = label(panel, "16 values kept", "bt-held-label");
          tag.hidden = true;
          panel.retained[m] = tag;
        }
        const x = m === "A" ? -2.4 : 2.4;
        const z = m === "B" ? -2.4 : 2.4;
        panel.names.push({
          el: label(panel, m),
          point: new THREE.Vector3(m === "A" ? -4.9 : 4.9, 0.1, z),
        });
        for (let r = 0; r < n; r++)
          for (let c = 0; c < n; c++) {
            const material = new THREE.MeshBasicMaterial({
              color: palette.idle,
            });
            const mesh = new THREE.Mesh(box, material);
            mesh.scale.set(side, 0.14, side);
            scene.add(mesh);
            const cell = {
              mesh,
              r,
              c,
              baseX: x,
              baseZ: z,
              pitch,
              side,
              first: 0,
              second: 0,
              kind: "idle",
              active: false,
              amount: 0,
            };
            if (n === 2)
              cell.el = label(
                panel,
                m === "C" ? "·" : String((m === "A" ? a : b)[r][c]),
                "tabular-nums"
              );
            panel.cells[m][r][c] = cell;
          }
        if (n === 8)
          for (let i = 0; i < 2; i++)
            for (let j = 0; j < 2; j++) {
              const frame = rectangle(1.84, 1.84);
              frame.visible = false;
              scene.add(frame);
              panel.frames[m].push({ frame, i, j, baseX: x, baseZ: z });
              panel.blockLabels[m].push({
                el: label(
                  panel,
                  m + sub[i] + sub[j],
                  "text-small bt-block-label"
                ),
                i,
                j,
                baseX: x,
                baseZ: z,
              });
            }
      }
      for (let i = 0; i < (size === "small" ? 4 : 48); i++) {
        const positions = new Float32Array(33 * 3),
          geometry = new THREE.BufferGeometry();
        geometry.setAttribute(
          "position",
          new THREE.BufferAttribute(positions, 3)
        );
        const line = new THREE.Line(
          geometry,
          new THREE.LineBasicMaterial({
            color: palette.first,
            transparent: true,
            opacity: 0.35,
          })
        );
        line.frustumCulled = false;
        scene.add(line);
        const dots = [0.065, 0.045, 0.025].map(radius => {
          const mesh = new THREE.Mesh(
            sphere,
            new THREE.MeshBasicMaterial({
              color: palette.first,
              transparent: true,
              depthWrite: false,
            })
          );
          mesh.scale.setScalar(radius);
          scene.add(mesh);
          return mesh;
        });
        panel.routes.push({ line, positions, dots, event: null, curve: null });
      }
      new ResizeObserver(() => {
        panel.width = host.clientWidth;
        panel.height = host.clientHeight;
        if (!panel.width || !panel.height) return;
        renderer.setSize(panel.width, panel.height, false);
        camera.left = -5.5;
        camera.right = 5.5;
        camera.top = (5.5 * panel.height) / panel.width;
        camera.bottom = -camera.top;
        camera.updateProjectionMatrix();
        render();
      }).observe(host);
      return panel;
    }
    let ready = false,
      time = 16.5,
      running = false,
      previous = 0,
      lastPhase = "",
      visible = false;
    new IntersectionObserver(([entry]) => {
      visible = entry.isIntersecting;
      previous = 0;
    }).observe(root);
    panels.small = makePanel("small");
    panels.large = makePanel("large");
    function emitPair(panel, start, duration, k, sources, target, gap = 0.4) {
      const contribution = {
        start,
        duration,
        arrival: start + duration,
        k: Number(k),
        target,
        gap,
        epoch: start >= BLOCK_START ? 2 : start >= 20 ? 1 : 0,
        weight: panel.n === 2 ? 4 : 1,
      };
      panel.contributions.push(contribution);
      for (const source of sources)
        panel.events.push({ ...contribution, source });
    }
    const small = panels.small,
      large = panels.large;
    for (let tick = 0; tick < 2; tick++) {
      const out = Math.floor(tick / 2),
        i = Math.floor(out / 2),
        j = out % 2,
        k = tick % 2;
      emitPair(
        small,
        BLOCK_START + tick * 4,
        3.225,
        k,
        [small.cells.A[i][k], small.cells.B[k][j]],
        small.cells.C[i][j]
      );
      for (let h = 0; h < 4; h++)
        for (let row = 0; row < 4; row++)
          for (let c = 0; c < 4; c++) {
            emitPair(
              large,
              BLOCK_START + tick * 4 + (h * 4 + row) * 0.175,
              0.6,
              k,
              [
                large.cells.A[4 * i + row][4 * k + h],
                large.cells.B[4 * k + h][4 * j + c],
              ],
              large.cells.C[4 * i + row][4 * j + c]
            );
          }
    }
    for (const panel of Object.values(panels))
      panel.events.sort((a, b) => a.start - b.start);
    function reset(panel) {
      for (const m of ["A", "B", "C"]) {
        panel.cells[m].flat().forEach(cell => {
          cell.kind = "idle";
          cell.active = false;
          cell.amount = 0;
          cell.first = 0;
          cell.second = 0;
        });
        panel.frames[m].forEach(f => (f.frame.visible = false));
      }
    }
    function tint(cell, k, active = false) {
      cell.kind = k ? "second" : "first";
      cell.active = active;
    }
    function accum(cell, first, second) {
      cell.kind = "sum";
      cell.first = first;
      cell.second = second;
      cell.amount = (first + second) / 8;
    }
    function applyArrivals(panel, t) {
      const epoch = t >= BLOCK_START ? 2 : t >= 20 ? 1 : 0;
      for (const cell of panel.cells.C.flat()) {
        cell.kind = "idle";
        cell.amount = 0;
        cell.first = 0;
        cell.second = 0;
        cell.value = 0;
        cell.started = false;
        cell.arrived = [false, false];
      }
      for (const event of panel.contributions) {
        if (event.epoch !== epoch || t < event.start) continue;
        const cell = event.target;
        cell.started = true;
        if (t < event.arrival) continue;
        const p = clamp((t - event.arrival) / LANDING_SECONDS, 0, 1),
          landed = p * p * (3 - 2 * p);
        if (event.k) cell.second += event.weight * landed;
        else cell.first += event.weight * landed;
        cell.arrived[event.k] = true;
        if (panel.n === 2)
          cell.value += a[cell.r][event.k] * b[event.k][cell.c];
      }
      for (const cell of panel.cells.C.flat()) {
        if (cell.first + cell.second > 0) accum(cell, cell.first, cell.second);
        if (cell.el)
          cell.el.textContent = cell.started ? String(cell.value) : "·";
      }
    }
    function tintBlock(m, i, j, k) {
      for (let r = 0; r < 4; r++)
        for (let c = 0; c < 4; c++)
          tint(panels.large.cells[m][4 * i + r][4 * j + c], k);
    }
    function hold(i, j, k) {
      panels.large.frames.A[2 * i + k].frame.visible = true;
      panels.large.frames.B[2 * k + j].frame.visible = true;
    }
    function sweep(t, terms, duration) {
      const p = clamp((t / duration) * terms, 0, terms),
        whole = Math.floor(p);
      return {
        cursor: Math.min(terms - 1, whole),
        count: whole + clamp((p - whole - 0.25) / 0.65, 0, 1),
        phase: p >= terms ? 1.2 : p - whole,
      };
    }
    function step(t) {
      const tick =
        t >= BLOCK_START
          ? Math.min(7, Math.floor((t - BLOCK_START) / 4))
          : t >= 20
            ? Math.min(1, Math.floor((t - 20) / 8))
            : Math.min(7, Math.floor(t / 2));
      const out = Math.floor(tick / 2),
        i = Math.floor(out / 2),
        j = out % 2,
        k = tick % 2;
      const scan =
        t >= BLOCK_START
          ? sweep(t >= BLOCK_START + 32 ? 4 : (t - BLOCK_START) % 4, 16, 2.8)
          : { cursor: 15, count: 16, phase: 1.2 };
      const full = Math.floor(scan.count / 4),
        rem = scan.count - full * 4;
      return {
        out,
        i,
        j,
        k,
        h: Math.floor(scan.cursor / 4),
        row: scan.cursor % 4,
        count: scan.count / 4,
        phase: scan.phase,
        rows: Array.from({ length: 4 }, (_, r) => full + clamp(rem - r, 0, 1)),
      };
    }
    const workingColour = new THREE.Color();
    function textColour(colour) {
      return 0.2126 * colour.r + 0.7152 * colour.g + 0.0722 * colour.b > 0.18
        ? palette.darkText
        : palette.lightText;
    }
    function boardPoint(baseX, x, y, z, baseZ = 0) {
      return new THREE.Vector3(baseX + x, y, baseZ + z);
    }
    function updateMeshes(panel, labels) {
      for (const m of ["A", "B", "C"]) {
        for (const cell of panel.cells[m].flat()) {
          const { mesh, r, c, pitch, baseX, baseZ } = cell;
          const split = panel.n === 8 ? panel.gap : 0;
          const localX =
            (c - (panel.n - 1) / 2) * pitch +
            (panel.n === 8 ? (c < 4 ? -split / 2 : split / 2) : 0);
          const localZ =
            (r - (panel.n - 1) / 2) * pitch +
            (panel.n === 8 ? (r < 4 ? -split / 2 : split / 2) : 0);
          mesh.position.copy(boardPoint(baseX, localX, 0, localZ, baseZ));
          let height = 0.13;
          if (cell.kind === "idle") workingColour.copy(palette.idle);
          else if (cell.kind === "sum") {
            const total = cell.first + cell.second;
            workingColour
              .copy(palette.first)
              .lerp(palette.second, total ? cell.second / total : 0);
            workingColour.lerp(palette.idle, 1 - (0.12 + 0.88 * cell.amount));
            height = 0.13 + 0.36 * cell.amount;
          } else {
            workingColour
              .copy(palette[cell.kind])
              .lerp(palette.idle, cell.active ? 0 : 0.3);
            height = cell.active ? 0.43 : 0.29;
          }
          mesh.material.color.copy(workingColour);
          if (panel.n === 8) height *= 0.5;
          mesh.scale.y = height;
          mesh.position.y = height / 2;
          if (cell.el) {
            cell.el.style.color = textColour(workingColour);
            projectLabel(
              panel,
              cell.el,
              new THREE.Vector3(mesh.position.x, height + 0.02, mesh.position.z)
            );
          }
        }
        for (const item of panel.frames[m]) {
          item.frame.position.copy(
            boardPoint(
              item.baseX,
              (item.j ? 1 : -1) * (0.88 + panel.gap / 2),
              0.16,
              (item.i ? 1 : -1) * (0.88 + panel.gap / 2),
              item.baseZ
            )
          );
          item.frame.material.color.copy(palette.fg);
        }
        const retained = panel.retained[m];
        if (retained) {
          const held = panel.frames[m].find(({ frame }) => frame.visible);
          retained.hidden = !held;
          if (held) {
            const point = held.frame.position.clone();
            point.z -= 1.13;
            projectLabel(panel, retained, point);
          }
        }
        for (const item of panel.blockLabels[m]) {
          item.el.style.display =
            labels && !panel.frames[m][2 * item.i + item.j].frame.visible
              ? ""
              : "none";
          item.el.style.color = textColour(
            panel.cells[m][4 * item.i + 1][4 * item.j + 1].mesh.material.color
          );
          projectLabel(
            panel,
            item.el,
            boardPoint(
              item.baseX,
              (item.j ? 1 : -1) * (0.88 + panel.gap / 2),
              0.3,
              (item.i ? 1 : -1) * (0.88 + panel.gap / 2),
              item.baseZ
            )
          );
        }
      }
      panel.names.forEach(item => projectLabel(panel, item.el, item.point));
    }
    function targetFocus(panel) {
      for (const m of ["A", "B", "C"]) {
        const cells =
          m === "C"
            ? panel.outputFocus
            : panel.cells[m].flat().filter(cell => cell.active);
        const focus = panel.focus[m];
        focus.cursor.visible = cells.length > 0;
        if (!cells.length) continue;
        const left =
          Math.min(...cells.map(cell => cell.mesh.position.x - cell.side / 2)) -
          0.035;
        const right =
          Math.max(...cells.map(cell => cell.mesh.position.x + cell.side / 2)) +
          0.035;
        const top =
          Math.min(...cells.map(cell => cell.mesh.position.z - cell.side / 2)) -
          0.035;
        const bottom =
          Math.max(...cells.map(cell => cell.mesh.position.z + cell.side / 2)) +
          0.035;
        focus.target = {
          x: (left + right) / 2,
          z: (top + bottom) / 2,
          width: right - left,
          height: bottom - top,
        };
        focus.cursor.material.color.copy(palette.fg);
      }
    }
    function moveFocus(panel, blend) {
      let moving = false;
      for (const focus of Object.values(panel.focus)) {
        if (!focus.target) continue;
        const { cursor, target } = focus,
          amount = reduced || !focus.initialized ? 1 : blend;
        for (const [holder, key, value] of [
          [cursor.position, "x", target.x],
          [cursor.position, "z", target.z],
          [cursor.scale, "x", target.width],
          [cursor.scale, "z", target.height],
        ]) {
          moving ||= Math.abs(holder[key] - value) > 0.001;
          holder[key] += (value - holder[key]) * amount;
        }
        focus.initialized = true;
      }
      return moving;
    }
    function eventPoint(panel, cell, gap) {
      const x =
        (cell.c - (panel.n - 1) / 2) * cell.pitch +
        (panel.n === 8 ? (cell.c < 4 ? -gap / 2 : gap / 2) : 0);
      const z =
        (cell.r - (panel.n - 1) / 2) * cell.pitch +
        (panel.n === 8 ? (cell.r < 4 ? -gap / 2 : gap / 2) : 0);
      return boardPoint(cell.baseX, x, 0.65, z, cell.baseZ);
    }
    function drawStreams(panel, t) {
      const live = panel.events.filter(
        event => t >= event.start && t < event.arrival + LANDING_SECONDS
      );
      const liveSet = new Set(live);
      for (const route of panel.routes)
        if (route.event && !liveSet.has(route.event)) route.event = null;
      for (const event of live) {
        if (panel.routes.some(route => route.event === event)) continue;
        const route = panel.routes.find(route => !route.event);
        if (!route) continue;
        route.event = event;
        const start = eventPoint(panel, event.source, event.gap),
          end = eventPoint(panel, event.target, event.gap),
          dx = end.x - start.x,
          dz = end.z - start.z;
        const arch = Math.min(0.65, Math.hypot(dx, dz) * 0.1);
        const c1 = start
            .clone()
            .add(new THREE.Vector3(dx * 0.3, 0.4, dz * 0.3 - arch)),
          c2 = end
            .clone()
            .add(new THREE.Vector3(-dx * 0.2, 0.4, -dz * 0.2 - arch));
        route.curve = new THREE.CubicBezierCurve3(start, c1, c2, end);
        for (let p = 0; p <= 32; p++) {
          route.curve.getPoint(p / 32, temp);
          temp.toArray(route.positions, p * 3);
        }
        route.line.geometry.attributes.position.needsUpdate = true;
      }
      for (const route of panel.routes) {
        const event = route.event,
          show = !!event;
        route.line.visible = show;
        route.dots.forEach(dot => (dot.visible = show && !reduced));
        if (!event) continue;
        const age = t - event.start,
          colour = event.k ? palette.second : palette.first;
        const fade =
          Math.min(1, age / 0.08) *
          clamp((event.arrival + LANDING_SECONDS - t) / 0.14, 0, 1);
        route.line.material.color.copy(colour);
        route.line.material.opacity = 0.18 * fade;
        route.dots.forEach((dot, tail) => {
          const raw = (age - tail * 0.035) / event.duration,
            p = clamp(raw, 0, 1),
            smooth = p * p * (3 - 2 * p);
          route.curve.getPoint(smooth, dot.position);
          dot.material.color.copy(colour);
          dot.material.opacity =
            fade * (tail === 0 ? 1 : tail === 1 ? 0.5 : 0.22);
          if (raw < 0 || raw > 1.06) dot.visible = false;
        });
      }
    }
    function render() {
      if (!ready) return;
      const s = step(time),
        small = panels.small,
        large = panels.large;
      small.outputFocus = time >= BLOCK_START ? [small.cells.C[s.i][s.j]] : [];
      large.outputFocus = [];
      reset(small);
      reset(large);
      if (time >= BLOCK_START) {
        tint(small.cells.A[s.i][s.k], s.k, true);
        tint(small.cells.B[s.k][s.j], s.k, true);
      }
      applyArrivals(small, time);
      applyArrivals(large, time);
      const complete = small.cells.C[s.i][s.j].arrived[s.k],
        first = a[s.i][0] * b[0][s.j],
        second = a[s.i][1] * b[1][s.j];
      root.querySelector("[data-equation]").textContent =
        time < BLOCK_START
          ? "Each entry below becomes a block."
          : `C${sub[s.i]}${sub[s.j]}: ${a[s.i][0]} × ${b[0][s.j]}${s.k ? ` + ${a[s.i][1]} × ${b[1][s.j]}` : ""}${complete ? " = " : " → "}${first + (s.k ? second : 0)}`;
      let phase,
        guidance,
        detail,
        labels = false;
      large.gap = 0.4;
      if (time < BLOCK_START) {
        phase = "Regrouping each matrix into four blocks.";
        guidance = "Each of the four entries is now a 4 × 4 matrix.";
        large.gap = 0.4 * clamp((time - 16.5) / 2.5, 0, 1);
        for (const m of ["A", "B"])
          for (let r = 0; r < 8; r++)
            for (let c = 0; c < 8; c++)
              tint(large.cells[m][r][c], (m === "A" ? c : r) >= 4);
        detail = "Each block contains 4 × 4 entries";
      } else {
        phase = "Multiplying the upper-left output block.";
        guidance =
          "The small reference and the large matrices follow the same step.";
        labels = true;
        tintBlock("A", s.i, s.k, s.k);
        tintBlock("B", s.k, s.j, s.k);
        hold(s.i, s.j, s.k);
        for (let q = 0; q < 4; q++) {
          tint(large.cells.A[4 * s.i + q][4 * s.k + s.h], s.k, true);
          tint(large.cells.B[4 * s.k + s.h][4 * s.j + q], s.k, true);
        }
        large.outputFocus = Array.from(
          { length: 4 },
          (_, q) => large.cells.C[4 * s.i + s.row][4 * s.j + q]
        );
        detail =
          time >= LOOP_END
            ? "Both pairs have contributed. The output block is complete."
            : `C${sub[s.i]}${sub[s.j]} · inner step ${s.h + 1}/4 · each input feeds 4 outputs`;
      }
      updateMeshes(small, false);
      updateMeshes(large, labels);
      targetFocus(small);
      targetFocus(large);
      moveFocus(small, 0);
      moveFocus(large, 0);
      drawStreams(large, time);
      drawStreams(small, time);
      for (const panel of Object.values(panels))
        panel.renderer.render(panel.scene, panel.camera);
      root.querySelector("[data-phase]").textContent = phase;
      root.querySelector("[data-guidance]").textContent = guidance;
      root.querySelector("[data-detail]").textContent = detail;
      timeline.value = time;
      const end = time < BLOCK_START ? 19.9999 : LOOP_END;
      play.textContent = running ? "Pause" : time >= end ? "Replay" : "Play";
      play.setAttribute("aria-pressed", String(running));
      root.dataset.chapter = time < BLOCK_START ? "1" : "3";
      status.textContent = running
        ? "Following the contributions"
        : "Choose a view, then press Play";
      if (phase !== lastPhase && root.dataset.view === "math") {
        root.querySelector("[data-announcement]").textContent = phase;
        lastPhase = phase;
      }
    }
    play.addEventListener("click", () => {
      const end = time < BLOCK_START ? 19.9999 : LOOP_END;
      if (time >= end) time = time < BLOCK_START ? 16.5 : BLOCK_START;
      running = !running;
      previous = 0;
      render();
    });
    timeline.addEventListener("input", () => {
      time = Number(timeline.value);
      running = false;
      previous = 0;
      render();
    });
    root.querySelectorAll("[data-seek]").forEach(button => {
      button.disabled = false;
      button.addEventListener("click", () => {
        root.dataset.view = "math";
        root.querySelector("[data-math-view]").hidden = false;
        root.querySelector("[data-cache-panel]").hidden = true;
        root.querySelector("cachet-blocking-cache").player?.stop();
        root
          .querySelectorAll("[data-seek], [data-cache-view]")
          .forEach(b => b.setAttribute("aria-pressed", String(b === button)));
        time = Number(button.dataset.seek);
        timeline.min = String(time);
        timeline.max = String(time < BLOCK_START ? 19.9999 : LOOP_END);
        running = false;
        previous = 0;
        render();
      });
    });
    root.addEventListener("cachet-view-change", () => {
      running = false;
      previous = 0;
      render();
    });
    function refreshTheme() {
      theme();
      render();
    }
    matchMedia("(prefers-color-scheme: dark)").addEventListener(
      "change",
      refreshTheme
    );
    new MutationObserver(refreshTheme).observe(document.documentElement, {
      attributes: true,
      attributeFilter: ["class", "style", "data-theme"],
    });
    play.disabled = false;
    timeline.disabled = false;
    ready = true;
    render();
    let focusPrevious = 0;
    function frame(now) {
      if (!root.isConnected) {
        for (const p of Object.values(panels)) p.renderer.dispose();
        return;
      }
      if (
        running &&
        visible &&
        !document.hidden &&
        root.dataset.view === "math"
      ) {
        const end = time < BLOCK_START ? 19.9999 : LOOP_END;
        if (previous)
          time = Math.min(end, time + Math.min((now - previous) / 1000, 0.1));
        if (time >= end) running = false;
        render();
      }
      const blend =
        1 - Math.exp(-Math.min((now - focusPrevious) / 1000, 0.06) / 0.09);
      focusPrevious = now;
      if (visible && !document.hidden && root.dataset.view === "math")
        for (const panel of Object.values(panels)) {
          if (moveFocus(panel, blend))
            panel.renderer.render(panel.scene, panel.camera);
        }
      previous = now;
      requestAnimationFrame(frame);
    }
    requestAnimationFrame(frame);
  } catch {
    status.textContent =
      "The animation could not start. The explanation below gives the same comparison.";
    status.setAttribute("role", "alert");
  }
}

// Defer the WebGL library and scenes until the reader approaches the figure.
const root = document.getElementById("blocking-reuse-three");
if (root) {
  const cacheButton = root.querySelector("[data-cache-view]");
  cacheButton.disabled = false;
  cacheButton.addEventListener("click", () => {
    root.dataset.view = "cache";
    root.querySelector("[data-math-view]").hidden = true;
    root.querySelector("[data-cache-panel]").hidden = false;
    root.querySelector("[data-announcement]").textContent =
      "Following the inputs through memory and cache.";
    root
      .querySelectorAll("[data-seek], [data-cache-view]")
      .forEach(b => b.setAttribute("aria-pressed", String(b === cacheButton)));
    root.dispatchEvent(new Event("cachet-view-change"));
  });
  const observer = new IntersectionObserver(
    ([entry]) => {
      if (entry.isIntersecting) {
        observer.disconnect();
        void initialize(root);
      }
    },
    { rootMargin: "300px" }
  );
  observer.observe(root);
}
