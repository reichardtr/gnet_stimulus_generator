import os
os.chdir('C:/Users/Ricsi/Documents/!!ResearchBMECOGSCI/visual_patterns/')

import csv
pics = []

with open('picturedata.csv', newline='') as csvfile:
    spamreader = csv.reader(csvfile, delimiter=' ', quotechar='|')
    for row in spamreader:
        pics.append(row)
        
import numpy as np        
coords = np.array([[2,-2,0],[2,0,0],[2,2,0],[0,-2,0],[0,0,0],[0,2,0],[-2,-2,0],[-2,0,0],[-2,2,0]],dtype=object)

colors = np.array([[1,0,0,1],[0,0,1,1],[0,1,0,1],[1,1,0,1],[1,0.3,0,1],[0.627,0.137,0.94,1]],dtype=object)

import bpy
import bpy.ops as o
import mathutils

scene = bpy.context.scene

# Create a light
light_data = bpy.data.lights.new('light', type='SPOT')
light = bpy.data.objects.new('light', light_data)
bpy.context.collection.objects.link(light)
light.location = mathutils.Vector((3, 3, 3))

# Create the camera
cam_data = bpy.data.cameras.new('kamcsi')
cam = bpy.data.objects.new('kamcsi', cam_data)
bpy.context.collection.objects.link(cam)
scene.camera = cam

cam.location = mathutils.Vector((7.25, -2, 7.2))
cam.rotation_euler = ((42.5 * 3.14/180), (-0.475 * 3.14/180), (72.5 * 3.14/180))
bpy.data.cameras['kamcsi'].lens = 30.0
# cam.rotation_euler = mathutils.Euler((0.9, 0.0, 1.0))
        
for i in range(1,330,3):
    # Select objects by type
    for o in bpy.context.scene.objects:
        if o.type == 'MESH':
            o.select_set(True)
        else:
            o.select_set(False)
    bpy.ops.object.delete()
    bpy.ops.mesh.primitive_plane_add(location=(0,0,-0.9), rotation=(0,0,0), size = 24)
    for j in range(1,10,1):
        if int(pics[i][0].split(',')[j]) == 1:
            if int(pics[i+2][0].split(',')[j]) == 1:
                bpy.ops.mesh.primitive_cube_add(location = coords[j-1],size=1.8) 
                obj = bpy.context.object
                obj.color = colors[int(pics[i+1][0].split(',')[j])-1]
                # Create a material
                mat = bpy.data.materials.new("material")
                # Activate its nodes
                mat.use_nodes = True
                # Get the principled BSDF (created by default)
                principled = mat.node_tree.nodes['Principled BSDF']
                # Assign the color
                principled.inputs['Base Color'].default_value = obj.color
                # Assign the material to the object
                obj.data.materials.append(mat)
            elif int(pics[i+2][0].split(',')[j]) == 3:
                bpy.ops.mesh.primitive_uv_sphere_add(location = coords[j-1],radius=0.9)
                obj = bpy.context.object
                obj.color = colors[int(pics[i+1][0].split(',')[j])-1]
                # Create a material
                mat = bpy.data.materials.new("material")
                # Activate its nodes
                mat.use_nodes = True
                # Get the principled BSDF (created by default)
                principled = mat.node_tree.nodes['Principled BSDF']
                # Assign the color
                principled.inputs['Base Color'].default_value = obj.color
                # Assign the material to the object
                obj.data.materials.append(mat)
            elif int(pics[i+2][0].split(',')[j]) == 2:
                bpy.ops.mesh.primitive_solid_add(source='4', size = 1.25)
                bpy.ops.transform.translate(value=[coords[j-1][0]-0.3,coords[j-1][1],coords[j-1][2]-0.45])
                # (-0.3, 2, -0.5)
                obj = bpy.context.object
                obj.color = colors[int(pics[i+1][0].split(',')[j])-1]
                # Create a material
                mat = bpy.data.materials.new("material")
                # Activate its nodes
                mat.use_nodes = True
                # Get the principled BSDF (created by default)
                principled = mat.node_tree.nodes['Principled BSDF']
                # Assign the color
                principled.inputs['Base Color'].default_value = obj.color
                # Assign the material to the object
                obj.data.materials.append(mat)
    scene.render.image_settings.file_format = 'PNG'
    scene.render.filepath = "c:/Users/Ricsi/Documents/!!ResearchBMECOGSCI/visual_patterns/"+pics[i][0].split(',')[0][1:-1]+".PNG"
    bpy.ops.render.render(write_still = True)