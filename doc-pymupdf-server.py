#!/usr/bin/env python
import fitz
import base64

from epc.server import EPCServer
from sexpdata import Symbol

server = EPCServer(('localhost', 0))

def normalize_edges(page, edges):
    "Transform vimura edges to (normalized) pdf-tools edges."
    size = doc[page - 1].mediabox_size
    return [edges[i]/size[0] if i in [0,2] else edges[i]/size[1] for i in range(0,4)]

def denormalize_edges(page, edges):
    "Transform (normalized) pdf-tools edges to vimura edges."
    size = doc[page - 1].mediabox_size
    return [edges[i]*size[0] if i in [0,2] else edges[i]*size[1] for i in range(0,4)]

# doc = fitz.open("/home/dalanicolai/test.pdf")

@server.register_function
def test(data):
    return data.value()

@server.register_function
def open(doc_file):
    global doc
    doc = fitz.open(doc_file)
    return False

# TODO implement?
@server.register_function
def close():
    pass

@server.register_function
def number_of_pages():
    return len(doc)

def parse_structured_text(d, t):
    match t:
        case "page":
            return [Symbol("page"), 0, 0, d['width'], d['height'],
                    *parse_structured_text(d['blocks'], "blocks")]
        case "blocks":
            return [[Symbol("block"),
                     *i['bbox'],
                     *parse_structured_text(i['lines'], "lines")]
                    for i in d if 'lines' in i]
        case "lines":
            return [[Symbol("line"),
                     *i['bbox'],
                     *parse_structured_text(i['spans'], "spans")]
                    for i in d if 'spans' in i]
        case "spans":
            return [[Symbol("span"),
                     *i['bbox'],
                     *parse_structured_text(i['chars'], "chars")]
                    for i in d if 'chars' in i]
        case "chars":
            return [[Symbol("char"), *i['bbox'], i['c']] for i in d]

@server.register_function
def page_structured_text(page=None, detail="words"):
    if detail == 'djvu':
        text = doc[page - 1].get_text('rawdict') if page else [p.get_text('rawdict') for p in doc]
    else:
        text = doc[page - 1].get_text(detail) if page else [p.get_text(detail) for p in doc]
    if detail == 'djvu':
        return parse_structured_text(text, "page")
    else:
        return text

@server.register_function
def pagesizes():
    return [list(p.mediabox_size) for p in doc]

@server.register_function
def renderpage_svg(page, text):
    p = doc[page - 1]
    return p.get_svg_image(fitz.Identity, bool(text))

@server.register_function
def renderpage_data(page, width, *args):
    p = doc[page - 1]
    if args:
        edges = fitz.Rect(denormalize_edges(page, args[2]))
        # edges = p.search_for("and")
        try:
            # p.add_highlight_annot(edges)
            p.draw_rect(edges, 0.5, 0.5, fill_opacity=0.5)
        except ValueError:
            print("Negelect this error")
    zoom = width/p.mediabox_size[0]
    mat = fitz.Matrix(zoom, zoom)
    pix = p.get_pixmap(matrix=mat)
    # p.clean_contents()
        # mag = display_width / pix.width
        # svg = page.get_svg_image(matrix=fitz.Matrix(mag, mag))
        # return pix.tobytes("ppm")
    return base64.b64encode(pix.tobytes("png")).decode()
    # return pix.tobytes("png")

def renderpage_file(page, width, path, *args):
    p = doc[page - 1]
    if args:
        edges = fitz.Rect(denormalize_edges(page, args[2]))
        # edges = p.search_for("and")
        try:
            # p.add_highlight_annot(edges)
            p.draw_rect(edges, 0.5, 0.5, fill_opacity=0.5)
        except ValueError:
            print("Negelect this error")
    zoom = width/p.mediabox_size[0]
    mat = fitz.Matrix(zoom, zoom)
    pix = p.get_pixmap(matrix=mat)
    # p.clean_contents()
        # mag = display_width / pix.width
        # svg = page.get_svg_image(matrix=fitz.Matrix(mag, mag))
    pix.save(path)

@server.register_function
def toc():
    return doc.get_toc()

@server.register_function
def metadata():
    return doc.metadata

@server.register_function
# def addannot(page, style, edges):
def addannot(page, style, edges):
    p = doc[page - 1]
    match style.value():
        case "highlight":
            p.add_highlight_annot(edges)
        case "line":
            p.add_line_annot(edges[0:2], edges[2:4])
    return edges
    # edges = fitz.Rect(denormalize_edges(page, edges))
    # p.add_highlight_annot(edges)
    # p.add_caret_annot(fitz.Rect(72, 72, 220, 100).tl)

@server.register_function
def editannot():
    pass

@server.register_function
def delannot():
    pass

# TODO create getannots function producing following response
# (((page . 1) (edges 0.15455 0.190049 0.335979 0.238749) (type . highlight) (id . annot-1-1) (flags . 0) (color . "#fefe00") (contents . "") (modified 25003 54259) (label . "Daniel Nicolai") (subject) (opacity . 1.0) (popup-edges) (popup-is-open) (created) (markup-edges (0.15455 0.190049 0.335979 0.238749))) ((page . 1) (edges 0.199907 0.131846 0.32086 0.180546) (type . highlight) (id . annot-1-0) (flags . 0) (color . "#fefe00") (contents . "") (modified 25003 54232) (label . "Daniel Nicolai") (subject) (opacity . 1.0) (popup-edges) (popup-is-open) (created) (markup-edges (0.199907 0.131846 0.32086 0.180546))))
@server.register_function
def getannots():
    return False

# TODO create pagelinks function producing following response
# (((edges 0.141183 0.14789 0.673246 0.16353) (type . goto-dest) (title . "") (page . 125) (top . 0.144794)) ((edges 0.217501 0.165013 0.735103 0.180717) (type . goto-dest) (title . "") (page . 125) (top . 0.402617)) ((edges 0.171309 0.182171 0.686421 0.197805) (type . goto-dest) (title . "") (page . 127) (top . 0.394724)) ((edges 0.141183 0.213566 0.374606 0.229207) (type . goto-dest) (title . "") (page . 129) (top . 0.144794)))
@server.register_function
def pagelinks():
    return False

@server.register_function
def save(filepath):
    doc.save(filepath)
    return filepath

@server.register_function
def get_annots(page):
    p = doc[page - 1]
    return [list(a.rect) for a in p.annots()]

@server.register_function
def get_contents():
    return p.get_contents()

@server.register_function
def get_drawings():
    return p.get_drawings()

@server.register_function
def getselection(page):
    p = doc[page]
    size = doc[page].mediabox_size
    return [[j[i]/size[0] if i in [0,2] else j[i]/size[1] for i in range(0,4)] for j in p.get_text("blocks")]

@server.register_function
def doublet_png(page, display_width):
    bottom_page = doc[page+1]
    shape = bottom_page.new_shape()
    line_start = bottom_page.rect.top_left
    line_end = bottom_page.rect.top_right
    shape.draw_line(line_start, line_end)
    shape.finish(color=(0, 0, 0), fill=(0, 0, 0))
    shape.commit()

    mag = display_width / bottom_page.rect.bottom_right.x
    mat = matrix=fitz.Matrix(mag, mag)

    pix = [doc[page].get_pixmap(matrix=mat),
           bottom_page.get_pixmap(matrix=mat)]

    tar_w = max(pix[0].width,pix[1].width)
    tar_h = pix[0].height + pix[1].height

    doublet = fitz.Pixmap(fitz.Colorspace(fitz.CS_RGB), (0, 0, tar_w, tar_h))

    for j in range(2):
        pix[j].set_origin(0, pix[j].height * j)
        doublet.copy(pix[j], pix[j].irect) # copy input to new loc

    imbytes = doublet.tobytes("ppm")  # extremely fast!

    imbytes64 = base64.b64encode(imbytes)
    return imbytes64.decode("utf-8")

@server.register_function
def page_svg(page, display_width):
    page = doc[page]
    pix = page.get_pixmap()
    mag = display_width / pix.width
    svg = page.get_svg_image(matrix=fitz.Matrix(mag, mag))
    # svg = page.get_svg_image(matrix=fitz.Identity)
    return svg

def svg_get_groups(svg):
    size = svg.split(" width=")[1].split(" viewBox")[0].split(" height=")
    trim_left = "<g".join(svg.split("<g")[1:])
    svg_groups = "</g>".join(trim_left.split("</g>"[:-1]))
    return [int(i) for i in size], svg_groups


@server.register_function
def page_triplet(page, display_width):
    # top, mid, bot = page-1, page, page+1
    p = [doc[page-1], doc[page], doc[page+1]]
    pix = [i.get_pixmap() for i in p]
    mag = [display_width / i.width for i in pix]
    svg_width = [v.width * mag[i] for i,v in enumerate(pix)]
    svg_height = [v.height * mag[i] for i,v in enumerate(pix)]
    start_height = 0

    def page_data(page, display_width):
        nonlocal start_height
        svg = [page.get_svg_image(matrix=fitz.Matrix(mag[0], mag[0])) for i, page in enumerate(p)]
        svg_body = []

        for i, svg in enumerate(svg):
            start = svg.find("<g ")
            end = svg.rfind("</g>")
            # svg_body.append(svg[start:end+len('</g>')])
            line = f"<line x1='0' y1='{start_height}' x2='{svg_width[i]}' y2='{start_height}' stroke='black'/>"
            g_string = f"<g transform=\"translate(0 {start_height})\" "
            # svg_body.append(svg[start:end+len('</g>')].replace("<g transform=\"", g_string, 1))
            svg_body.append(line + svg[start:end+len('</g>')].replace("<g ", g_string, 1))
            start_height += svg_height[i]
        d = (f'<svg width="{svg_width[0]}" height="{svg_height[0]*len(svg_body)}" '
             f'version="1.1" xmlns="http://www.w3.org/2000/svg" '
             f'xmlns:xlink="http://www.w3.org/1999/xlink"></svg>'
             )
        svg_head_tail = d.split("><")

        s = svg_head_tail[0] + '>' \
            + " <rect width='100%' height='100%' fill='white'/>\n" \
            + ''.join(svg_body) \
            + '<' + svg_head_tail[1]
        return svg_height, s

    return page, page_data(page, display_width)

    # svg = page.get_svg_image(matrix=fitz.Identity)
    # size = svg.split(" width=")[1].split(" viewBox")[0].split(" height=")
    # pages = [doc[1], doc[2], doc[3]]
    # pages_svg = [i.get_svg_image() for i in pages]
    # page_size_string = [svg_size(i) for i in pages_svg]
    # pages_data = [page_data(i, display_width) for i in [page-1, page, page+1]]

    # float(page_size_string[0][1][1:-3])

# def start():
#     server.print_port()
#     server.serve_forever()

server.print_port()
server.serve_forever()
